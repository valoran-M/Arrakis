(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format
open Common.Print

(*
  File managing errors at the entry point of Arrakis.
  It will display errors properly in the terminal.
*)

(* Error Printing  ---------------------------------------------------------- *)

let init e =
  let open Init in
  match e with
  | No_Input_File ->
    eprintf "%a Please specify an input file@." error ();
    exit 1
  | Input_File_Dont_Exist f ->
    eprintf "%a Specified input file @{<fg_yellow>'%s'@} doesn't exist@."
      error () f;
    exit 2
  | Invalid_env s ->
    eprintf "%a Invalid environment @{<fg_yellow>'%s'@}@." error () s;
    exit 7
  | Running_Root_Without_Opt ->
    eprintf "%a Running in root mode is not allowed!@." error ();
    eprintf "@{<fg_yellow>Tip:@} Use --allow-root if you know what you are doing@.";
    exit 9
  | Too_Much_Input_File ->
    let inps = List.map (fun s -> "'"^s^"'") Options.input_file in
    eprintf "%a Too much input file specified!@." error ();
    eprintf "I got @{<fg_yellow>%s@} but expected only one@." (String.concat " " inps);
    exit 10

let assembler line e =
  let open Assembler.Error in
  match e with
  | Lexing_error s ->
    eprintf "%a Lexical error on line @{<fg_yellow>%d@}: '%s'@." error ()
      line s;
    exit 3
  | Parsing_error s ->
    eprintf "%a Syntax error on line @{<fg_yellow>%d@}: '%s'@." error ()
      line s;
    exit 4
  | Unknown_Label ul ->
    eprintf "%a Unknown label on line @{<fg_yellow>%d@}: '%s'@." error ()
      line ul;
    exit 5
  | Interval_imm (v, min, max) ->
    eprintf "%a on line @{<fg_yellow>%d@}: Immediate out of bound" error () line;
    eprintf "Found @{<fg_yellow>'%ld'@} but expected a value between %ld and %ld@."
      v min max;
    exit 6

