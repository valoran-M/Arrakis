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

