(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format
open Error
open Common.Pp

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
  | []              -> cmd.execute args state
  | command :: args ->
      try parse_command command args cmd.sub state
      with Not_found -> cmd.execute args state

let auto_complete (s : Types.state) (i: Io.Std.t) =
  let open Common in
  let input = Cstring.before i.s in
  let cmds  = Utils.all_commands_string all_commands in
  let cmds  = List.filter (String.starts_with ~prefix:input) cmds in
  match cmds with
  | []  -> i
  | [c] -> { i with s = Cstring.add_string c (Cstring.backspace i.s.cursor i.s) }
  | _   ->
    fprintf s.out_channel "\n%!";
    List.iter (fun c -> printf "%-20s" c) cmds;
    fprintf s.out_channel "\n%!";
    i

let exec_command (s : Types.state) line =
  match String.split_on_char ' ' line with
  | []          -> s
  | cmd :: args ->
    try parse_command cmd args s.cmds s
    with
    | Not_found ->
      fprintf s.out_channel "%a Undefined command @{<fg_red>'%s'@}.@\n"
        error () cmd;
      fprintf s.out_channel "%a Try @{<fg_green>'help'@}.@."
        hint ();
      s
    | Shell_error Bad_Usage ->
      fprintf s.out_channel "%a Bad usage @{<fg_red>'%s'@}.@\n"
        error () line;
      fprintf s.out_channel "%a Try @{<fg_green>'help'@}.@."
        hint ();
      s

let start (state : Types.state) =
  let rec loop (s : Types.state) (i: Io.Std.t) =
    try
      match s.input i with
      | Exit   -> ()
      | Tab t  -> auto_complete s t |> loop s
      | Line (l, i) ->
        if l = ""
        then loop s { i with s = Common.Cstring.empty }
        else loop (exec_command state l) { i with s = Common.Cstring.empty }
    with Quit.Shell_Exit | End_of_file -> ()
  in
  ignore (state.init "> ");
  loop state { s = Common.Cstring.empty; h = Io.History.empty };
  ignore (state.exit ())

let run (state : Types.state) (args : string list) =
  ignore (Running.run_execute args state)
