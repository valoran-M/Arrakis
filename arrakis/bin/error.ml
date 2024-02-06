(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Format

(*
  This file is here to manage errors at the entry point of Arrakis.

  It's split into 2 parts :
  - Declaration of user interface errors
  - Error display functions
*)

(* UI Error ----------------------------------------------------------------- *)

type main_error =
  | Invalid_env of string
  | No_Input_File
  | Too_Much_Input_File
  | Input_File_Dont_Exist of string
  | Running_Root_Without_Opt

exception Main_error of main_error

(* Error Printing  ---------------------------------------------------------- *)

let error_main error =
  match error with
  | No_Input_File ->
    eprintf "@{<fg_red>Error:@} Please specify an input file.@.";
    exit 1
  | Input_File_Dont_Exist f ->
    eprintf "@{<fg_red>Error:@} Specified input file '%s' doesn't exist.@." f;
    exit 2
  | Invalid_env s ->
    eprintf "@{<fg_red>Error:@} Invalid environment @{<fg_yellow>'%s'@}@." s;
    exit 7
  | Running_Root_Without_Opt ->
    eprintf "@{<fg_red>Error:@} Running in root mode is not allowed!@.";
    eprintf "@{<fg_yellow>Tip:@} Use --allow-root if you know what you are doing.@.";
    exit 9
  | Too_Much_Input_File ->
    let inps = List.map (fun s -> "'"^s^"'") Options.input_file in
    eprintf "@{<fg_red>Error:@} Too much input file specified!@.";
    eprintf "I got @{<fg_yellow>%s@} but expected only one.@." (String.concat " " inps);
    exit 10

let error_assembly line error =
  let open Assembler.Error in
  match error with
  | Lexing_error s ->
    eprintf "@{<fg_red>Error:@} Lexical error on line @{<fg_yellow>%d@}: '%s'@."
      line s;
    exit 3
  | Parsing_error s ->
    eprintf "@{<fg_red>Error:@} Syntax error on line @{<fg_yellow>%d@}: '%s'@."
      line s;
    exit 4
  | Unknown_Label ul ->
    eprintf "@{<fg_red>Error:@} Unknown label on line @{<fg_yellow>%d@}: '%s'@."
      line ul;
    exit 5
  | Interval_imm (v, min, max) ->
    eprintf
      "@{<fg_red>Error:@} on line @{<fg_yellow>%d@}: Imm out of bound.
        Found @{<fg_yellow>'%s'@} but expected a value between %s and %s@}@."
      line (Int32.to_string v) (Int32.to_string min) (Int32.to_string max);
    exit 6


let error_simulator error =
  let open Simulator.Error in
  match error with
  | Conversion_Failure ->
    eprintf "@{<fg_red>Error:@} Couldn't convert an int32 to an int. @.";
    eprintf "Time to move to a 64 bit machine!";
    exit 8
  | _ -> assert false (* TODO *)

