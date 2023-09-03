open Options
open Simulator
open Format
open Assembler.Error
open Color

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
    then Server.start_server unix_file
    else Shell.shell arch label debug
  with
  | Lexing_error (ln, s) ->
      eprintf "%sLexical error on line %d: %s\n" c_red ln s;
      exit 1
  | Assembler.Parser.Error  ->
      eprintf "%sSyntax error!\n" c_red;
      exit 2
  | Assembler_error (ln, Unknown_Label ul) ->
      eprintf "%sUnknown label on line %d: %s" c_red ln ul;
      exit 3
  | Assembler_error (ln, Interval_imm _) ->
      eprintf "%sError on line %d" c_red ln;
      exit 4
