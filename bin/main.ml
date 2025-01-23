(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Options
open Format

let version = "1.2.0-dev"

(* main *)
let () =
  try
    let opt = Options.get () in
    if opt.show_version then (printf "%s@." version; exit 0);

    Init.colors opt;
    Init.check_root opt;
    let in_file = Init.get_input_file opt in
    let syscall = Init.get_ecall opt in

    let channel = open_in in_file in
    let lb = Lexing.from_channel channel in
    let mem, labels, debug = Assembler.Assembly.assembly lb in
    let pc = Assembler.Label.get_global labels "_start" in

    let arch  = Arch.Riscv.init pc mem in
    let shell = Shell.create arch syscall debug labels (Option.is_none opt.run) in

    match opt.run with
    | Some args -> Shell.run   shell args
    | None      -> Shell.start shell
  with
  | Init.Init_error e                      -> Error.init e
  | Assembler.Error.Assembler_error (l, e) -> Error.assembler l e

