(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Options
open Format

let version = "1.1.2-dev"

(* main *)
let () =
  try
    let opt = Options.get () in
    if opt.show_version then (printf "%s@." version; exit 0);

    Init.colors opt;
    Init.check_root opt;
    let in_file = Init.get_input_file opt in
    let ecall = Init.get_ecall opt in

    let channel = open_in in_file in
    let lb = Lexing.from_channel channel in
    let mem, labels, debug = Assembler.Assembly.assembly lb in
    let pc = Assembler.Label.get_global labels "_start" in
    let arch  = Arch.Riscv.init pc mem in

    match opt.run with
    | Some a -> Shell.run   (Shell.create_run arch ecall debug labels) a
    | None   -> Shell.start (Shell.create     arch ecall debug labels)
  with
  | Init.Init_error e                      -> Error.init e
  | Common.Error.Common_error e            -> Error.common e
  | Assembler.Error.Assembler_error (l, e) -> Error.assembler l e

