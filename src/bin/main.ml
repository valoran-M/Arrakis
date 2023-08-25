open Options

let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

open Simulator

let () =
  let channel = open_in input_file in
  let mem, _, label =
    Assembler.Translate.translate (Lexing.from_channel channel)
  in
  let arch = Arch.init (Assembler.Segment.text_begin) mem in
  Shell.shell arch label
