(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format

let error fmt () =
  fprintf fmt "@{<bold>@{<fg_red>Error:@}@}"

let info fmt () =
  fprintf fmt "@{<fg_blue>Info:@}"

let warning fmt () =
  fprintf fmt "@{<fg_yellow>Warning:@}"
