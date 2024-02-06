(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(*
  File conatining all the functions used to initialise Arrakis :
  - colors
  - input file
  - super user check
  - syscall initialisation
*)

(* Errors ------------------------------------------------------------------- *)

type init_error =
  | Invalid_env of string
  | No_Input_File
  | Too_Much_Input_File
  | Input_File_Dont_Exist of string
  | Running_Root_Without_Opt

exception Init_error of init_error

(* Actual initialisation ---------------------------------------------------- *)

let colors_init () =
  if not Options.no_color then Colorsh.setup_std ()

let get_input_file () =
  let file =
    match Options.input_file with
    | []        -> raise (Init_error No_Input_File)
    | hd :: []  -> hd
    | _         -> raise (Init_error Too_Much_Input_File)
  in
  if not (Sys.file_exists file)
  then raise (Init_error (Input_File_Dont_Exist file));
  file

let check_root () =
  match Unix.getuid (), Options.allow_root with
  | 0, true  ->
    Format.eprintf
      "@{<fg_yellow>Warning: Running in root mode. Proceed with caution.@}@."
  | 0, false -> raise (Init_error Running_Root_Without_Opt)
  | _, _     -> ()

let init_syscall () =
  let syscall =
    match Options.env with
    | "unix"  -> Syscall.Scunix.syscall
    | "venus" -> Syscall.Scvenus.syscall
    | s       -> raise (Init_error (Invalid_env s))
  in
  if not Options.unix_socket
  then Syscall.Sutils.set_stdout Unix.stdin Unix.stdout Unix.stderr;
  syscall

