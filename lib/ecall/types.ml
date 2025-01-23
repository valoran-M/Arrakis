(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type ecall_ret = Exit of int | Continue

type ecall = Format.formatter -> Arch.Riscv.t -> ecall_ret
