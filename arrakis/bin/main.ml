open Options
open Simulator
open Format
open Assembler.Error
open Simulator.Error

exception Invalid_env of string

let () =
  if Options.no_color
  then ()
  else Color.setup ()

let () =

  if input_file = "" then (
    print_endline usage;
    exit 1
  );

  if Unix.getuid () == 0 then (
    if allow_root then (
      printf
      "@{<fg_yellow>Warning: Running in root mode. Proceed with caution.@}@."
    ) else (
      eprintf "@{<fg_red>Error:@} Running in root mode is not allowed!@." ;
      eprintf "@{<fg_yellow>Tip:@} Use --allow-root if you know what you are doing.@.";
      exit 2
    )
  );

  try

  (* Initialise syscall ----------------------------------------------------- *)

  let syscall =
    match Options.env with
    | "unix"  -> Syscall.Scunix.syscall
    | "venus" -> Syscall.Scvenus.syscall
    | s       -> raise (Invalid_env s)
  in

  if not unix_socket then (
    Syscall.Utils.set_stdout Unix.stdin Unix.stdout Unix.stderr
  );

  (* ------------------------------------------------------------------------ *)

  let channel = open_in input_file in
  let lb = Lexing.from_channel channel in
    let mem, label, global_label, addr_debug, line_debug =
      Assembler.Translate.translate lb
    in
    let history = History.create_history () in
    let pc =
      try Hashtbl.find global_label "_start"
      with Not_found -> Simulator.Segment.text_begin
    in
    let arch = Arch.init pc mem in
    if unix_socket
    then Server.start_server unix_file arch history label
                             addr_debug line_debug syscall
    else if no_shell then (
      let channel = Format.std_formatter in
      Shell.program_run := true;
      ignore (Shell.run false channel arch history syscall)
    )
    else Shell.shell arch history label addr_debug line_debug syscall
  with
  | Assembler_error (ln, Lexing_error s) ->
      eprintf
        "@{<fg_red>Error:@}Lexical error on line @{<fg_yellow>%d@}: '%s'@." ln s;
      exit 3
  | Assembler_error(ln, Parsing_error(s)) ->
      eprintf
        "@{<fg_red>Error:@} Syntax error on line @{<fg_yellow>%d@}: '%s'@." ln s;
      exit 4
  | Assembler_error (ln, Unknown_Label ul) ->
      eprintf
        "@{<fg_red>Error:@} Unknown label on line @{<fg_yellow>%d@}: '%s'@." ln ul;
      exit 5
  | Assembler_error (ln, Interval_imm (v, min, max)) ->
      eprintf "@{<fg_red>Error:@} on line @{<fg_yellow>%d@}: Imm out of bound.
      Found @{<fg_yellow>'%s'@} but expected a value between %s and %s@}@." ln
      (Int32.to_string v) (Int32.to_string min) (Int32.to_string max);
      exit 6
  | Failure s ->
      eprintf "@{<fg_red>Error: @} Failure (@{<fg_yellow>'%s'@}@." s;
      exit 6
  | Invalid_env s ->
      eprintf "@{<fg_red>Error:@} Invalid environment @{<fg_yellow>'%s'@}@." s;
      exit 7
  | Simulator_error Conversion_Failure ->
      eprintf "@{<fg_red>Error:@} Couldn't convert an int32 to an int. @.";
      eprintf "Time to move to a 64 bit machine!";
      exit 8

