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
    let mem, _, label, global_label, addr_debug, line_debug =
      Assembler.Translate.translate (Lexing.from_channel channel)
    in
    let pc =
      if   Hashtbl.mem global_label "main"
      then Hashtbl.find global_label "main"
      else Simulator.Segment.text_begin
    in
    let arch = Arch.init pc mem in
    if unix_socket
    then Server.start_server unix_file arch label addr_debug line_debug
    else Shell.shell arch label addr_debug line_debug
  with
  | Lexing_error (ln, s) ->
      eprintf "@{<fg_red>Lexical error on line @{<fg_yellow>%d@}: %s@}@." ln s;
      exit 3
  | Assembler.Parser.Error  ->
      eprintf "@{<fg_red>Syntax error!@}@.";
      exit 4
  | Assembler_error (ln, Unknown_Label ul) ->
      eprintf "@{<fg_red>Unknown label on line @{<fg_yellow>%d@}: %s@}@." ln ul;
      exit 5
  | Assembler_error (ln, Interval_imm _) ->
      eprintf "@{<fg_red>Error on line @{<fg_yellow>%d@}@}@." ln;
      exit 6
