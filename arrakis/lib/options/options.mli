(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

val spec : (string * Arg.spec * string) list

val show_version : bool

val unix_socket : bool
val unix_file   : string

val usage      : string
val input_file : string list

val no_color   : bool
val allow_root : bool

val env        : string
val just_run   : bool
