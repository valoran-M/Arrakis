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
  Running.next;
  Help.help;
  Print.print;
  Breakpoint.breakpoint;
  Quit.quit;
]

let create arch syscall debug labels : Types.state =
  let cmds = Hashtbl.create (List.length all_commands) in
  List.iter (fun (cmd : Types.cmd) ->
    Hashtbl.add cmds cmd.short_form cmd;
    Hashtbl.add cmds cmd.long_form  cmd)
  all_commands;
  {
    (* Shell state *)
    out_channel  = Format.std_formatter;
    cmds;
    cmds_history = [||];
    breakpoints  = Hashtbl.create 64;
    program_end  = false;

    (* Program state *)
    history = Simulator.History.create_history ();
    arch;
    debug;
    labels;
    syscall;
  }

let parse_command args (state : Types.state) cmd =
  match Hashtbl.find_opt state.cmds cmd with
  | Some cmd -> cmd.execute args state
  | None     ->
    fprintf state.out_channel "%a Undefined command: @{<fg_yellow>'%s'@}. "
      error () cmd;
    fprintf state.out_channel "Try @{<fg_green>'help'@}.@.";
    state

let rec start (state : Types.state) =
  fprintf state.out_channel "> %!";
  let line  = read_line ()                  in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    let new_state = parse_command args state command in
    start new_state
  | _ -> start state
  with Quit.Shell_Exit -> ()

let run (state : Types.state) =
  ignore (Running.run_execute [] state)

