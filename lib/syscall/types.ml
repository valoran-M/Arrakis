(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type syscall_ret = Exit of int | Continue

type syscall = Format.formatter -> Arch.Riscv.t -> syscall_ret
