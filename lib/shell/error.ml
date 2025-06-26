(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)


open Format
open Common.Pp

type t =
  | Bad_Usage
  | Cmd_Not_Found

exception Shell_error of t

let cmd_not_found (s : Types.state) line =
  fprintf s.out_channel "%a Undefined command @{<fg_red>'%s'@}.@\n"
    error () line;
  fprintf s.out_channel "%a Try @{<fg_green>'help'@}.@."
    hint ()

let cmd_bad_usage (s : Types.state) all_cmd line =
  fprintf s.out_channel "%a Bad usage @{<fg_red>'%s'@}.@\n"
  error () line;
  fprintf s.out_channel "%a Try @{<fg_green>'help %s'@}.@."
  hint () all_cmd

let eraise (e : t) =
    raise @@ Shell_error e
