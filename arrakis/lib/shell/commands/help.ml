(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Format

let print (name : string) (desc : string) (sub : Types.cmd list) (state : Types.state) =
  fprintf state.out_channel "%2s@{<fg_green>%s@}\n" "" name;
  (if desc != "" then fprintf state.out_channel "%2s%s\n" "" desc);
  List.iter (fun (cmd : Types.cmd) ->
    fprintf state.out_channel "%2s@{<fg_green>*@} %-15s%2s%s\n"
    "" cmd.name "" cmd.short_desc)
  sub;
  state

let general (state : Types.state) =
  print "General" "" state.cmds state

let command (cmd : Types.cmd) (state : Types.state) =
  print (String.capitalize_ascii cmd.long_form) cmd.long_desc cmd.sub state

let execute args (state : Types.state) =
  match args with
  | []       -> general state
  | hd :: _  ->
    try               command (List.find (Utils.cmd_eq hd) state.cmds) state
    with Not_found -> general state

let help : Types.cmd = {
  long_form   = "help";
  short_form  = "h";
  name        = "(h)elp";
  short_desc  = "Show this help";
  long_desc   = "";
  execute     = execute;
  sub         = [];
}

