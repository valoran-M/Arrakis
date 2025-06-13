(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format
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
  let open Common in
  let input = Cstring.before i.s in
  let cmds  = Utils.all_commands_sl all_commands in
  let cmds  = List.filter (String.starts_with ~prefix:input) cmds in
  match cmds with
  | []  -> i
  | [c] -> { i with s = Cstring.add_string c (Cstring.backspace i.s.cursor i.s) }
  | _   ->
    fprintf s.out_channel "\n%!";
    List.iter (fun c -> fprintf s.out_channel "%-20s" c) cmds;
    fprintf s.out_channel "\n%!";
    i

let exec_command (s : Types.state) line =
  try
    let cmd, all_cmd, args = Utils.get_cmd line s in
    try cmd.execute args s
    with Shell_error Bad_Usage   ->
      let all_cmd = (Utils.all_commands_string all_cmd) in
      cmd_bad_usage s all_cmd line
  with Shell_error Cmd_Not_Found -> cmd_not_found s line

let start (state : Types.state) =
  let rec loop (i: Io.Std.t) =
    try
      match state.input i with
      | Exit  -> ()
      | Tab t -> auto_complete state t |> loop
      | Line (l, i) ->
        exec_command state l;
        loop { i with s = Common.Cstring.empty }
    with Quit.Shell_Exit | End_of_file -> ()
  in
  ignore (state.init ());
  loop Io.Std.empty;
  ignore (state.exit ())

let run (state : Types.state) (args : string list) =
  ignore (Running.run_execute args state)

