(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Format

let all_commands = [
  Running.reset;
  Running.prev;
  Running.step;
  Help.help;
  Print.print;
  Breakpoint.breakpoint;
  Quit.quit;
]

let create arch syscall debug labels : Types.state =
  let commands = Hashtbl.create 64 in
  List.iter (fun (cmd : Types.command) ->
    Hashtbl.add commands cmd.short_form cmd;
    Hashtbl.add commands cmd.long_form cmd)
  all_commands;
  {
    out_channel = Format.std_formatter;

    history     = Simulator.History.create_history ();
    arch;
    debug;
    labels;

    breakpoints = Hashtbl.create 64;
    program_run = false;
    program_end = false;
    syscall;

    commands;
  }

let parse_command args (state : Types.state) command =
  match Hashtbl.find_opt state.commands command with
  | Some cmd -> cmd.execute args state
  | None     ->
      Format.fprintf state.out_channel
      "@{<fg_red>Error:@} Undefined command: @{<fg_yellow>'%s'@}. \
      Try @{<fg_green>'help'@}.@." command; state

let rec run (state : Types.state) =
  fprintf state.out_channel "> %!";
  let line  = read_line ()                  in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    let new_state = parse_command args state command in
    run new_state
  | _ -> run state
  with Quit.Shell_Exit -> ()

