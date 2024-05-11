(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions.Insts

type text_line =
  | Text_Pseudo of int * string * pseudo_inst          (* line, original code *)
  | Text_Instr  of int * string * basics_inst          (* line, original code *)
  | Text_GLabel of int * string                        (* line, label         *)
  | Text_Label  of string

type text = text_line list

type data_line =
  | Data_Ascii  of string list
  | Data_Asciz  of string list
  | Data_Bytes  of expr list
  | Data_GLabel of int * string
  | Data_Label  of string
  | Data_Word   of expr list
  | Data_Zero   of int32

type data = (data_line) list                              (* line, data *)

type t = {
  data : data;
  text : text;
}

