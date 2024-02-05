(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Options
open Format

let version = "1.0.1-dev"

let main =
  try
    if show_version then printf "%s@." version else

    (* Init ----------------------------------------------------------------- *)
    Init.colors_init ();
    Init.check_root  ();
    let input_file = Init.get_input_file () in
    let syscall = Init.init_syscall () in

    (* Main ----------------------------------------------------------------- *)

    let channel = open_in input_file in
    let lb = Lexing.from_channel channel in
      let mem, labels, debug = Assembler.Assembly.assembly lb in
      let history = Simulator.History.create_history () in
      let pc =
        match Assembler.Label.get_global labels "_start" with
        | Some pc -> pc
        | None    -> Simulator.Segment.text_begin
      in
      let arch = Simulator.Arch.init pc mem in
      if unix_socket
      then Server.start_server unix_file arch history labels debug syscall
      else if just_run then (
        let channel = Format.std_formatter in
        Shell.program_run := true;
        ignore (Shell.run false channel arch history syscall)
      )
      else Shell.shell arch history labels debug syscall
  with
  | Error.Main_error error ->
    Error.error_main error
  | Assembler.Error.Assembler_error (line, error) ->
    Error.error_assembly line error
  | Simulator.Error.Simulator_error error ->
    Error.error_simulator error

