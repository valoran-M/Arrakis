(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type init_error =
  | Invalid_env of string
  | No_Input_File
  | Too_Much_Input_File
  | Input_File_Dont_Exist of string
  | Running_Root_Without_Opt

exception Init_error of init_error

val colors : unit -> unit

val input_file : unit -> string

val check_root : unit -> unit

val syscall :
  unit -> Format.formatter -> Arch.Riscv.t -> Syscall.Types.syscall_ret

