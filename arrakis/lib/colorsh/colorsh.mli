(******************************************************************************)
(*  Copyright 2023-2024 Gurvan Debaussart                                     *)
(*  This file is distributed under the MIT license.                           *)
(*  https://codeberg.org/gurvan/colorsh                                       *)
(******************************************************************************)

val setup_fmt : Format.formatter -> unit
  (** [setup_fmt fmt] Active every color tags on [fmt] **)

val setup_std : unit -> unit
  (** [setup_std] Activate color tag on stdout and stderr. **)
