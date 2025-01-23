(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format
open Error
open Common.Print

let all_commands = [
  Running.finish;
  Running.previous;
  Running.next;
  Running.step;
  Running.run;
  Running.continue;
  Help.help;
  Info.info;
  Breakpoint.breakpoint;
  Quit.quit;
]

let create arch ecall debug labels run : Types.state =
  { (* Shell state *)
    out_channel  = Format.std_formatter;
    cmds         = all_commands;
    cmds_history = [||];
    breakpoints  = Hashtbl.create 64;
    program_run  = false;

    (* Program state *)
    history = History.create_history run;
    arch;
    debug;
    labels;
    ecall; }

let rec parse_command command args cmds state =
  let cmd = List.find (Utils.cmd_eq command) cmds in
  match args with
  | []                -> cmd.execute args state
  | command' :: args' ->
      try parse_command command' args' cmd.sub state
      with Not_found -> cmd.execute args state

let rec start (state : Types.state) =
  try
    fprintf state.out_channel "> %!";
    let line  = read_line ()                  in
    let words = String.split_on_char ' ' line in
    match words with
    | []              -> start state
    | command :: args ->
      let new_state =
        try parse_command command args state.cmds state
        with
        | Not_found ->
          fprintf state.out_channel "%a Undefined command @{<fg_yellow>'%s'@}. "
            error () command;
          fprintf state.out_channel "Try @{<fg_green>'help'@}.@.";
          state
        | Shell_error Bad_Usage ->
          fprintf state.out_channel "%a Bad usage @{<fg_yellow>'%s'@}. "
            error () line;
          fprintf state.out_channel "Try @{<fg_green>'help'@}.@.";
          state
      in
      start new_state
  with Quit.Shell_Exit | End_of_file -> ()

let run (state : Types.state) (args : string list) =
  ignore (Running.run_execute args state)
