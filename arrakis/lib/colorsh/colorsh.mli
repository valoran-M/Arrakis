(******************************************************************************)
(*  Colorsh                                                                   *)
(*  License: MIT                                                              *)
(*  Author:  Gurvan Debaussart                                                *)
(*  Source:  https://codeberg.org/gurvan/colorsh                              *)
(******************************************************************************)

val setup_fmt : Format.formatter -> unit
  (** [setup_fmt fmt] Active every color tags on [fmt] **)

val setup_std : unit -> unit
  (** [setup_std] Activate color tag on stdout and stderr. **)
