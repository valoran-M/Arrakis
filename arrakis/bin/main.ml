open Options
open Simulator

let () =
  let channel = open_in input_file in
  let mem, _, label, debug =
    Assembler.Translate.translate (Lexing.from_channel channel)
  in
  let arch = Arch.init (Simulator.Segment.text_begin) mem in
  Shell.shell arch label debug
