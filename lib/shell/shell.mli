(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

val create : Arch.Riscv.t -> Ecall.Types.ecall -> Assembler.Debug.t ->
  Assembler.Label.t -> bool -> Types.state

val run    : Types.state -> string list -> unit
val start  : Types.state -> unit
