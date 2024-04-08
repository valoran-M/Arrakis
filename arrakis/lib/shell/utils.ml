(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

exception Shell_invalid_arg of string list

let cmd_eq (command : string) (cmd : Types.cmd) =
  cmd.long_form = command || cmd.short_form = command
