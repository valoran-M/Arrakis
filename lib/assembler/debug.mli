(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type t

(** Create a debug interface *)
val generate_debug : unit -> t

val add_line_to_addr : t -> int -> int32 -> unit

val get_addr : t -> int -> int32

val add_addr_to_line : t -> int32 -> int -> string -> unit

val get_line : t -> int32 -> int * string
