(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Options
open Format

(* Entry point to Arrakis *)

let version = "1.1.0-dev"

let main =
  try
    if show_version then (printf "%s@." version; exit 0);

    (* Init ----------------------------------------------------------------- *)

    Init.colors_init ();
    Init.check_root  ();
    let input_file = Init.get_input_file () in
    let syscall    = Init.init_syscall   () in

    let channel = open_in input_file in
    let lb = Lexing.from_channel channel in
    let mem, labels, debug = Assembler.Assembly.assembly lb in
    let pc =
      match Assembler.Label.get_global labels "_start" with
      | Some pc -> pc
      | None    -> Arch.Segment.text_begin
    in
    let arch  = Arch.Riscv.init pc mem in
    let shell = Shell.create arch syscall debug labels in

    (* Main ----------------------------------------------------------------- *)

    Shell.run shell
      (* if unix_socket *)
      (* then Server.start_server unix_file arch history labels debug syscall *)
      (* else if just_run then ( *)
      (*   let channel = Format.std_formatter in *)
      (*   Shell.program_run := true; *)
      (*   ignore (Shell.run false channel arch history syscall) *)
      (* ) *)
      (* else Shell.shell arch history labels debug syscall *)
  with
  | Init.Init_error e                      -> Error.init e
  | Assembler.Error.Assembler_error (l, e) -> Error.assembler l e
  | Sim_utils.Error.Simulator_error e      -> Error.simulator e

