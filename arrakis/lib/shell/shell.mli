(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

val create : Arch.Riscv.t -> Syscall.Types.syscall -> Assembler.Debug.t ->
  Assembler.Label.t -> Types.state

val run    : Types.state -> unit
