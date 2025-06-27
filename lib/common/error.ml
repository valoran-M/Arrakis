(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type t =
  | Conversion_Failure

exception Common_error of t

let eraise e =
  raise (Common_error e)
