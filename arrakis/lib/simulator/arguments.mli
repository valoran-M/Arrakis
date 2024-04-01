(******************************************************************************)
(* copyright 2023-2024 - arrakis contributors                                 *)
(*                                                                            *)
(* this file is part of arrakis, a risc-v simulator.                          *)
(* it is distributed under the cecill 2.1 license <http://www.cecill.info>    *)
(******************************************************************************)


val write_arguments : Arch.Riscv.t -> string list -> unit
(** [set_arguments arch args] write programme set_arguments in memory

    args := a1 :: a2 :: ... :: \[\]
 *)
