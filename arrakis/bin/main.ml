open Options
open Simulator
open Format
open Assembler.Error

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
      eprintf "@{<fg_red>Error: Running in root mode is not allowed!@}@." ;
      eprintf "@{<fg_yellow>Tip: Use --allow-root if you know what you are doing.@}@.";
      exit 2
    )
  );

  try
  let channel = open_in input_file in
  let lb = Lexing.from_channel channel in
    let mem, label, global_label, addr_debug, line_debug =
      Assembler.Translate.translate lb
    in
    let pc =
      try Hashtbl.find global_label "main"
      with Not_found -> Simulator.Segment.text_begin
    in
    let arch = Arch.init pc mem in
    if unix_socket
    then Server.start_server unix_file arch label addr_debug line_debug
    else if no_shell then (
      let channel = Format.std_formatter in
      Shell.program_run := true;
      Shell.run false channel arch
    )
    else Shell.shell arch label addr_debug line_debug
  with
  | Assembler_error (ln, Lexing_error s) ->
      eprintf
        "@{<fg_red>Lexical error on line @{<fg_yellow>%d@}: '%s'@}@." ln s;
      exit 3
  | Assembler_error(ln, Parsing_error(s)) ->
      eprintf
        "@{<fg_red>Syntax error on line @{<fg_yellow>%d@}: '%s'@}@." ln s;
      exit 4
  | Assembler_error (ln, Unknown_Label ul) ->
      eprintf
        "@{<fg_red>Unknown label on line @{<fg_yellow>%d@}: '%s'@}@." ln ul;
      exit 5
  | Assembler_error (ln, Interval_imm (v, min, max)) ->
      eprintf "@{<fg_red>Error on line @{<fg_yellow>%d@}: Imm out of bound.
      Found %s but expected a value between %s and %s@}@." ln
      (Int32.to_string v) (Int32.to_string min) (Int32.to_string max);
      exit 6
  | Failure s ->
      eprintf "@{<fg_red>Failure: @} %s@." s;
      exit 6
