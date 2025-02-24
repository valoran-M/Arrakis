(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type 'a t

val empty : 'a t

val to_rope : 'a -> 'a t
(** [to_rope i] transform [i] in rope *)

val concat : 'a t -> 'a t -> 'a t
(** [concat l r] concat [l] and [r] in constant time *)

val to_list : 'a t -> 'a list
(** [to_list r] return [r] into list *)

