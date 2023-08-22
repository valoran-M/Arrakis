let usage_msg = "arrakis <file>"

let input_file = ref ""
let anon_fun filename = input_file := filename

let speclist = []

let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

open Simulator

let () =
  Arg.parse speclist anon_fun usage_msg;
  let channel = open_in !input_file in
  let mem, _ = Assembler.Translate.translate (Lexing.from_channel channel) in
  let arch = Arch.init (Assembler.Segment.text_begin) mem in
  Shell.run arch

