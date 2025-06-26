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
      "" cmd.name "" cmd.short_desc) sub;
  pp_print_flush state.out_channel ()

let general (state : Types.state) =
  print "General" [] state.cmds state

let command_title (title : string) (cmd : Types.cmd) (state : Types.state) =
  print (String.capitalize_ascii title) cmd.long_desc cmd.sub state

let command (cmd : Types.cmd) (state : Types.state) =
  command_title cmd.long_form cmd state

let execute args (state : Types.state) =
  match args with
  | [] -> general state
  | _  ->
    let c, _, _ = Utils.get_cmd_l args state in
    command c state

let help : Types.cmd =
  { long_form   = "help";
    short_form  = "h";
    name        = "(h)elp";
    short_desc  = "Get help on a command";
    long_desc   = ["Usage: help <command> <sub_command>"];
    execute     = execute;
    sub         = [];
    args        = Nil; }
