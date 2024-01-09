(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

type t

val make : unit -> t
  (** Creates a memory area of size 2**32 *)

val direct_access : t -> Bytes.t

val get_byte : t -> int32 -> int32
  (** [get_byte memory addr] Get Byte in [memory]
      at the addresse [addr] *)

val set_byte : t -> int32 -> int32 -> unit
  (** [set_byte memory addr value] Set [value] (Byte size)
      in [memory] at the addresse [addr] *)


val get_int16 : t -> int32 -> int32
  (** [get_int16 memory addr] Get int16 in [memory]
      at the addresse [addr] *)

val set_int16 : t -> int32 -> int32 -> unit
  (** [set_int16 memory addr value] Set [value] (2 Byte size)
      in [memory] at the addresse [addr] *)


val get_int32 : t -> int32 -> int32
  (** [get_int32 memory addr] Get int32 in [memory]
      at the addresse [addr] *)

val set_int32 : t -> int32 -> int32 -> unit
  (** [set_int32 memory addr value] Set [value] (4 Byte size)
      in [memory] at the addresse [addr] *)

val set_32b_zero : t -> int32 -> int32 -> unit
  (** [set_32b_zero memory addr nz] set [nz] zero at [addr] int [memory] *)


val get_str : t -> int32 -> string
  (** [get_str memory addr] *)

val set_str : t -> int32 -> string -> int -> unit
  (** [set_str memory addr value] *)

