(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(* UI Error ----------------------------------------------------------------- *)

type main_error =
  | Invalid_env of string
  | No_Input_File
  | Too_Much_Input_File
  | Input_File_Dont_Exist of string
  | Running_Root_Without_Opt

exception Main_error of main_error

(* Error Printing ----------------------------------------------------------- *)

val error_main : main_error -> unit

val error_assembly : int -> Assembler.Error.t -> unit

val error_simulator : Simulator.Error.t -> unit

