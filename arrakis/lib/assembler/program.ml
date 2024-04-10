(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions.Insts

type text_line =
                (* line nb, original code *)
  | Text_Pseudo of int      * string      * pseudo_inst
  | Text_Instr  of int      * string      * basics_inst
  | Text_GLabel of int      * string
  | Text_Label  of string

type text = text_line list

type data_line =
  | Data_Zero   of int32
  | Data_Bytes  of char list
  | Data_Ascii  of string list
  | Data_Asciz  of string list
  | Data_Word   of int32 list
  | Data_GLabel of int * string
  | Data_Label  of string

type data = data_line list

type t = {
    data : data;
    text : text;
}

