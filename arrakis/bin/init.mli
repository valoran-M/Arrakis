(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

val colors_init : unit -> unit

val get_input_file : unit -> string

val check_root : unit -> unit

val init_syscall :
  unit -> Format.formatter -> Simulator.Arch.t -> Syscall.Types.syscall_ret

