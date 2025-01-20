(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

val spec : (string * Arg.spec * string) list

val show_version : bool

val usage      : string
val input_file : string list

val no_color   : bool
val allow_root : bool

val env        : string
val run        : string list option
