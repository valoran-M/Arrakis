(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Error

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
    init         = Io.Std.init;
    exit         = Io.Std.exit;
    input        = Io.Std.read_line;
    out_channel  = Format.std_formatter;
    cmds         = all_commands;
    breakpoints  = Hashtbl.create 64;
    program_run  = false;

    (* Program state *)
    history = History.create_history run;
    arch;
    debug;
    labels;
    ecall; }

let auto_complete (s : Types.state) (i : Io.Std.t) =

  let complete last l_cmds =
    let open Common in
    let cmds = Utils.all_commands_sl l_cmds in
    let cmds = List.filter (String.starts_with ~prefix:last) cmds in
    match cmds with
    | []  -> i
    | [c] -> { i with s = Cstring.complete i.s (String.length last) c }
    | _   -> Utils.print_cmds cmds s; i
  in

  let input = Common.Cstring.before i.s in
  if String.trim input = ""
  then complete "" s.cmds
  else
    let c, ls = Utils.list_rm_last (String.split_on_char ' ' input) in
    try
      let cmd, _, args = Utils.get_cmd_l c s in
      if args = [] then complete ls cmd.sub else i
    with Shell_error Cmd_Not_Found -> complete ls s.cmds

let exec_command (s : Types.state) line =
  try
    let cmd, all_cmd, args = Utils.get_cmd (String.trim line) s in
    try cmd.execute args s
    with Shell_error Bad_Usage   ->
      let cmd = Utils.all_commands_string all_cmd in
      cmd_bad_usage s cmd line
  with Shell_error Cmd_Not_Found -> cmd_not_found s line

let start (state : Types.state) =
  let open Io.Std in
  let rec loop (i: Io.Std.t) =
    try match state.input i with
    | Exit        -> ()
    | Tab t       -> auto_complete state t |> loop
    | Line (l, i) -> exec_command state l; loop (rm_string i)
    with Quit.Shell_Exit -> ()
  in
  ignore (state.init ());
  loop empty;
  ignore (state.exit ())

let run (state : Types.state) (args : string list) =
  ignore (Running.run_execute args state)

