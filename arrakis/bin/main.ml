(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Options
open Simulator
open Format
open Assembler.Error
open Simulator.Error

exception Invalid_env of string
exception No_Input_File
exception Too_Much_Input_File
exception Input_File_Dont_Exist of string
exception Running_Root_Without_Opt

let version = "1.0.1-dev"

let () =
  if not Options.no_color then Colorsh.setup_std ()

let main =
  if show_version then printf "%s@." version else
  try

  let input_file =
    match input_file with
    | []        -> raise No_Input_File
    | hd :: []  -> hd
    | _         -> raise Too_Much_Input_File
  in

  (if not (Sys.file_exists input_file)
    then raise (Input_File_Dont_Exist input_file));

  begin match Unix.getuid (), allow_root with
  | 0, true  -> printf "@{<fg_yellow>Warning: Running in root mode. Proceed with caution.@}@."
  | 0, false -> raise Running_Root_Without_Opt
  | _, _     -> ()
  end;

  (* Init syscalls ---------------------------------------------------------- *)

  let syscall =
    match Options.env with
    | "unix"  -> Syscall.Scunix.syscall
    | "venus" -> Syscall.Scvenus.syscall
    | s       -> raise (Invalid_env s)
  in

  if not unix_socket then (
    Syscall.Utils.set_stdout Unix.stdin Unix.stdout Unix.stderr
  );

  (* ------------------------------------------------------------------------ *)

  let channel = open_in input_file in
  let lb = Lexing.from_channel channel in
    let mem, label, global_label, debug = Assembler.Assembly.assembly lb in
    let history = History.create_history () in
    let pc =
      try Hashtbl.find global_label "_start"
      with Not_found -> Simulator.Segment.text_begin
    in
    let arch = Arch.init pc mem in
    if unix_socket
    then Server.start_server unix_file arch history label debug syscall
    else if just_run then (
      let channel = Format.std_formatter in
      Shell.program_run := true;
      ignore (Shell.run false channel arch history syscall)
    )
    else Shell.shell arch history label debug syscall
  with
  | No_Input_File ->
      eprintf "@{<fg_red>Error:@} Please specify an input file.@.";
      exit 1
  | Input_File_Dont_Exist f ->
      eprintf "@{<fg_red>Error:@} Specified input file '%s' doesn't exist.@." f;
      exit 2
  | Assembler_error (ln, Lexing_error s) ->
      eprintf
        "@{<fg_red>Error:@} Lexical error on line @{<fg_yellow>%d@}: '%s'@." ln s;
      exit 3
  | Assembler_error (ln, Parsing_error s) ->
      eprintf
        "@{<fg_red>Error:@} Syntax error on line @{<fg_yellow>%d@}: '%s'@." ln s;
      exit 4
  | Assembler_error (ln, Unknown_Label ul) ->
      eprintf
        "@{<fg_red>Error:@} Unknown label on line @{<fg_yellow>%d@}: '%s'@." ln ul;
      exit 5
  | Assembler_error (ln, Interval_imm (v, min, max)) ->
      eprintf "@{<fg_red>Error:@} on line @{<fg_yellow>%d@}: Imm out of bound.
      Found @{<fg_yellow>'%s'@} but expected a value between %s and %s@}@." ln
      (Int32.to_string v) (Int32.to_string min) (Int32.to_string max);
      exit 6
  | Invalid_env s ->
      eprintf "@{<fg_red>Error:@} Invalid environment @{<fg_yellow>'%s'@}@." s;
      exit 7
  | Simulator_error Conversion_Failure ->
      eprintf "@{<fg_red>Error:@} Couldn't convert an int32 to an int. @.";
      eprintf "Time to move to a 64 bit machine!";
      exit 8
  | Running_Root_Without_Opt ->
      eprintf "@{<fg_red>Error:@} Running in root mode is not allowed!@.";
      eprintf "@{<fg_yellow>Tip:@} Use --allow-root if you know what you are doing.@.";
      exit 9
  | Too_Much_Input_File ->
      let inps = List.map (fun s -> "'"^s^"'") input_file in
      eprintf "@{<fg_red>Error:@} Too much input file specified!@.";
      eprintf "I got @{<fg_yellow>%s@} but expected only one.@." (String.concat " " inps);
      exit 10
  | Failure s ->
      eprintf "@{<fg_red>Error: @} Failure (@{<fg_yellow>'%s'@}@." s;
      exit 100

