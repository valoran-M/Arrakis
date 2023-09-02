open Options
open Simulator

let () =
  if input_file = "" then (
    print_endline usage;
    exit 1;
  );
  let channel = open_in input_file in
  let mem, _, label, debug =
    Assembler.Translate.translate (Lexing.from_channel channel)
  in
  let arch = Arch.init (Simulator.Segment.text_begin) mem in
  if !unix_socket
  then Server.start_server !unix_file
  else Shell.shell arch label debug
