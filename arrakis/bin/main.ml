(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Options
open Format

(*
   Entry point to Arrakis
*)

let version = "1.2.0-dev"

let main =
  try
    if show_version then (printf "%s@." version; exit 0);

    Init.colors ();
    Init.check_root ();
    let in_file = Init.input_file () in
    let syscall = Init.syscall () in

    let channel = open_in in_file in
    let lb = Lexing.from_channel channel in
    let mem, labels, debug = Assembler.Assembly.assembly lb in
    let pc = Assembler.Label.get_global labels "_start" in

    let arch  = Arch.Riscv.init pc mem in
    let shell = Shell.create arch syscall debug labels (Option.is_none run) in

    match run with
    | Some args -> Shell.run   shell args
    | None      -> Shell.start shell
  with
  | Init.Init_error e                      -> Error.init e
  | Assembler.Error.Assembler_error (l, e) -> Error.assembler l e

