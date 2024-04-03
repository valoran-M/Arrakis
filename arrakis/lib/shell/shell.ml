(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Format
open Global_utils.Print

let all_commands = [
  Running.reset;
  Running.prev;
  Running.step;
  Running.run;
  Running.continue;
  Help.help;
  Print.print;
  Breakpoint.breakpoint;
  Quit.quit;
]

let create arch syscall debug labels : Types.state =
  { (* Shell state *)
    out_channel  = Format.std_formatter;
    cmds         = all_commands;
    cmds_history = [||];
    breakpoints  = Hashtbl.create 64;
    program_end  = false;

    (* Program state *)
    history = History.create_history ();
    arch;
    debug;
    labels;
    syscall; }

let rec parse_command command args cmds state =
  let cmd = List.find (Utils.cmd_eq command) cmds in
  match args with
  | []            -> cmd.execute args state
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
        with Not_found ->
          fprintf state.out_channel "%a Undefined command @{<fg_yellow>'%s'@}. "
            error () command;
          fprintf state.out_channel "Try @{<fg_green>'help'@}.@.";
          state
      in
      start new_state
  with Quit.Shell_Exit | End_of_file -> Printf.printf "\nGoodbye :)\n"

let run (state : Types.state) =
  ignore (Running.run_execute [] state)

