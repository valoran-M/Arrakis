open Options
open Simulator
open Format
open Assembler.Error

let () =
  Color.setup ()

let () =
  if input_file = "" then (
    print_endline usage;
    exit 1;
  );
  try
    let channel = open_in input_file in
    let mem, _, label, debug =
      Assembler.Translate.translate (Lexing.from_channel channel)
    in
    let arch = Arch.init (Simulator.Segment.text_begin) mem in
    if unix_socket
    then Server.start_server unix_file arch label debug
    else Shell.shell arch label debug
  with
  | Lexing_error (ln, s) ->
      eprintf "@{<fg_red>Lexical error on line %d: %s@}@." ln s;
      exit 1
  | Assembler.Parser.Error  ->
      eprintf "@{<fg_red>Syntax error!@}@.";
      exit 2
  | Assembler_error (ln, Unknown_Label ul) ->
      eprintf "@{<fg_red>Unknown label on line %d: %s@}@." ln ul;
      exit 3
  | Assembler_error (ln, Interval_imm _) ->
      eprintf "@{<fg_red>Error on line %d@}@." ln;
      exit 4
