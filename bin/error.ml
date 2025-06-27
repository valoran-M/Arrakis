(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format
open Common.Pp

(*
  Manage and display errors
*)

(* Error Printing  ---------------------------------------------------------- *)

let init e =
  let open Init in
  match e with
  | No_Input_File ->
      eprintf "%a Please specify an input file@." error ();
      exit 1
  | Input_File_Dont_Exist f ->
      eprintf "%a Input file @{<fg_red>'%s'@} doesn't exist@."
        error () f;
      exit 2
  | Invalid_env s ->
      eprintf "%a Invalid environment @{<fg_red>'%s'@}@." error () s;
      exit 7
  | Running_Root_Without_Opt ->
      eprintf "%a Running in root mode is not allowed!\n" error ();
      eprintf "%a Use --allow-root if you know what you are doing@." info ();
      exit 9
  | Too_Much_Input_File fs ->
      let inps =
        List.map (fun s -> "'"^s^"'") fs
      |> String.concat " "
      in
      eprintf "%a Too much input files!@\n" error ();
      eprintf "I got @{<fg_red>%s@} but expected only one@." inps;
      exit 10

let common e =
  let open Common.Error in
  match e with
  | Int32_of_string s ->
      eprintf "%a Could not convert string @{<fg_red>%s@} to Int32@." error () s;
      exit 1
  | Int32_unsigned_to_int i ->
      eprintf "%a Could not convert unsigned Int32 @{<fg_red>%ld@} to int@." error () i;
      exit 1

let assembler line e =
  let open Assembler.Error in
  match e with
  | Lexing_error s ->
      eprintf "%a Lexical error on line @{<fg_red>%d@}: '%s'@." error ()
        line s;
      exit 3
  | Parsing_error s ->
      eprintf "%a Syntax error on line @{<fg_red>%d@}: '%s'@." error ()
        line s;
      exit 4
  | Unknown_Label ul ->
      eprintf "%a Unknown label on line @{<fg_red>%d@}: '%s'@." error ()
        line ul;
      exit 5
  | Interval_imm (v, min, max) ->
      eprintf "%a on line @{<fg_red>%d@}: Immediate out of bound" error () line;
      eprintf "Found @{<fg_red>'%ld'@} but expected a value between %ld and %ld@."
        v min max;
      exit 6

