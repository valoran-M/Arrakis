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

let auto_complet (s : Types.state) (i: Io.Std.t) =
  let input = Common.Cstring.befor i.s in
  let cmds  = Utils.all_commands_string all_commands in
  let cmds  = List.filter (String.starts_with ~prefix:input) cmds in
  let open Common.Cstring in
  match cmds with
  | []  -> i
  | [c] -> { i with s = add_string c (backspace i.s.cursor i.s) }
  | _   ->
    fprintf s.out_channel "\n%!";
    List.iter (fun c -> Printf.printf "%-20s" c) cmds;
    fprintf s.out_channel "\n%!";
    i

let exec_command (s : Types.state) line =
  let words = String.split_on_char ' ' line in
  match words with
  | []          -> s
  | cmd :: args ->
    try parse_command cmd args s.cmds s
    with
    | Not_found ->
      fprintf s.out_channel "%a Undefined command @{<fg_yellow>'%s'@}."
        error () cmd;
      fprintf s.out_channel "Try @{<fg_green>'help'@}.@.";
      s
    | Shell_error Bad_Usage ->
      fprintf s.out_channel "%a Bad usage @{<fg_yellow>'%s'@}."
        error () line;
      fprintf s.out_channel "Try @{<fg_green>'help'@}.@.";
      s

let start (state : Types.state) =
  let rec loop (s : Types.state) (i: Io.Std.t) =
    try
      match s.input i with
      | Exit   -> ()
      | Tab t  -> auto_complet s t |> loop s
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

