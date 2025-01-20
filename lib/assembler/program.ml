(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Instructions.Insts

type global_directive =
  | Glob_GLabel of int * string                        (* line, label         *)
  | Glob_Size   of int * string * expr                 (* line, label, expr   *)

type text_line =
  | Text_Pseudo of int * string * pseudo_inst          (* line, original code *)
  | Text_Instr  of int * string * basics_inst          (* line, original code *)
  | Text_Label  of string
  | Text_Direc  of global_directive

type text = text_line list

type data_line =
  | Data_Ascii of string list
  | Data_Asciz of string list
  | Data_Bytes of expr list
  | Data_Label of string
  | Data_Word  of expr list
  | Data_Zero  of int32
  | Data_Direc of global_directive

type data = (data_line) list

type t = {
  data : data;
  text : text;
}

