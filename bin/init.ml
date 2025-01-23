(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Common.Print
open Options

(*
  File containing all the functions used to initialise Arrakis :
  - colors
  - input file
  - super user security check
  - ecall initialisation
*)

(* Errors ------------------------------------------------------------------- *)

type init_error =
  | Invalid_env of string
  | No_Input_File
  | Too_Much_Input_File of string list
  | Input_File_Dont_Exist of string
  | Running_Root_Without_Opt

exception Init_error of init_error

let eraise e =
    raise (Init_error e)

(* Actual initialisation ---------------------------------------------------- *)

let colors opt =
  if not opt.no_color then Colorsh.setup_std ()

let get_input_file opt =
  let file =
    match opt.input_file with
    | hd :: []  -> hd
    | []        -> eraise No_Input_File
    | _         -> eraise (Too_Much_Input_File opt.input_file)
  in
  if not (Sys.file_exists file)
  then raise (Init_error (Input_File_Dont_Exist file));
  file

let check_root opt =
  match Unix.getuid (), opt.allow_root with
  | 0, true  ->
    Format.eprintf
      "%a Running in root mode. Proceed with caution.@}@." warning ()
  | 0, false -> eraise Running_Root_Without_Opt
  | _        -> ()

let get_ecall opt =
  let ecall =
    match opt.env with
    | "unix"  -> Ecall.Eunix.ecall
    | "venus" -> Ecall.Evenus.ecall
    | s       -> eraise (Invalid_env s)
  in
  Ecall.Utils.set_stdout Unix.stdin Unix.stdout Unix.stderr;
  ecall

