(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

type t

val add_address     : t -> string -> int32 -> unit
val get_address     : t -> string -> int   -> int32
val get_address_opt : t -> string -> int32 option
val made_global     : t -> string -> int   -> unit
val get_global      : t -> string -> int32 option

val get_label_address : Program.program -> t

