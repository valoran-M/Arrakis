(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Arch

type return =
  | Sys_call of History.t
  | Zero
  | Continue of int32 * History.t

val exec_instruction : Riscv.t -> History.t -> return
