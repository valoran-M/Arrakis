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

let auto_completion (s : Types.state) (i : Io.Std.t) =

  let complete_cmd last cmds =
    Utils.complete last (Utils.all_commands_sl cmds) i s
  in

  let complete_arg last args (cmd : Types.cmd) =
    match Utils.get_type_next_arg args cmd.args with
    | None -> i
    | Some a ->
      match a with
      | Nil -> i
      | Regs -> Utils.complete last (Assembler.Regs.all_regs) i s
      (* TODO *)
      | Text_addr _ -> i
      | Text_size _ -> i
      | Data_addr _ -> i
      | Data_size _ -> i
  in

  let input = Common.Cstring.before i.s in
  if String.trim input = ""
  then complete_cmd "" s.cmds
  else
    let c, ls = Utils.list_rm_last (String.split_on_char ' ' input) in
    try
      let cmd, _, args = Utils.get_cmd_l c s in
      if cmd.args = Nil
      then complete_cmd ls cmd.sub
      else complete_arg ls args cmd
    with Shell_error Cmd_Not_Found -> complete_cmd ls s.cmds

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
    | Tab t       -> auto_completion state t |> loop
    | Line (l, i) -> exec_command state l; loop (rm_string i)
    with Quit.Shell_Exit -> ()
  in
  ignore (state.init ());
  loop empty;
  ignore (state.exit ())

let run (state : Types.state) (args : string list) =
  ignore (Running.run_execute args state)

