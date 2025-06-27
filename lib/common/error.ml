(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type t =
  | Int32_of_string         of string
  | Int32_unsigned_to_int   of int32

exception Common_error of t

let eraise e =
  raise (Common_error e)
