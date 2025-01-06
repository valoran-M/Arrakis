(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Types

let complete (cmd : string) (cmds : cmd list) =
  let ok = String.starts_with ~prefix:cmd in
  let long  = List.filter (fun (cmd : cmd) -> ok cmd.long_form)  cmds in
  let short = List.filter (fun (cmd : cmd) -> ok cmd.short_form) cmds in
  short @ long

(* Read a line from stdin:
  - Auto complete when '\t' is read
  - Go back in history with arrows
  - Allow erasing the current line with backspace
  - Coloring when a command is valid (green) or invalid (red)
*)
let line (_state : Types.state)  : string =
  (* TODO *)
  read_line ()

