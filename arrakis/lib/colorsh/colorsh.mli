(** colorsh - *****************************************************************)
(*  Copyright 2023-2024 Gurvan Debaussart (https://debauss.art)               *)
(*  This file is distributed under the MIT license                            *)
(******************************************************************************)

val setup_fmt : Format.formatter -> unit
  (** [setup_fmt fmt] Active every color tags on [fmt] **)

val setup_std : unit -> unit
  (** [setup_std] Activate color tag on stdout and stderr. **)
