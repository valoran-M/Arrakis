(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format

let print (name : string) (desc : string list) (sub : Types.cmd list) (state : Types.state) =
  fprintf state.out_channel "%2s@{<fg_green>%s@}\n" "" name;
  List.iter (fprintf state.out_channel "%2s%s\n" "") desc;
  List.iter (fun (cmd : Types.cmd) ->
    fprintf state.out_channel "%2s@{<fg_green>*@} %-15s%2s%s\n"
      "" cmd.name "" cmd.short_desc)
  sub;
  state

let general (state : Types.state) =
  print "General" [] state.cmds state

let command_title (title : string) (cmd : Types.cmd) (state : Types.state) =
  print (String.capitalize_ascii title) cmd.long_desc cmd.sub state

let command (cmd : Types.cmd) (state : Types.state) =
  command_title cmd.long_form cmd state

let rec sub_command args title (cmd : Types.cmd) (state : Types.state) =
  let title = title^cmd.long_form^" " in
  match args with
  | []       -> command_title title cmd state
  | hd :: tl ->
    try
      sub_command tl title (List.find (Utils.cmd_eq hd) cmd.sub) state
    with Not_found -> command_title title cmd state

let execute args (state : Types.state) =
  match args with
  | []       -> general state
  | hd :: tl ->
    try
      sub_command tl "" (List.find (Utils.cmd_eq hd) state.cmds) state
    with Not_found -> general state

let help : Types.cmd =
  { long_form   = "help";
    short_form  = "h";
    name        = "(h)elp";
    short_desc  = "Get help on a command";
    long_desc   = ["Usage: help <command> <sub_command>"];
    execute     = execute;
    sub         = []; }
