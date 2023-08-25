let usage_msg = "arrakis <file>"

let input_file = ref ""
let anon_fun filename = input_file := filename

let speclist = []

open Simulator

let () =
  Arg.parse speclist anon_fun usage_msg;
  let channel = open_in !input_file in
  let mem, _, label, debug =
    Assembler.Translate.translate (Lexing.from_channel channel)
  in
  let arch = Arch.init (Assembler.Segment.text_begin) mem in
  Shell.shell arch label debug

