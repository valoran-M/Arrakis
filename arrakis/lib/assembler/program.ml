(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions.Insts

type text_line =
                (* line nb, original code *)
  | Text_Pseudo of int      * string      * pseudo_instruction
  | Text_Instr  of int      * string      * instruction
  | Text_GLabel of int      * string
  | Text_Label  of string

(* Memory ------------------------------------------------------------------- *)

type data_line =
  | Data_Zero   of int32
  | Data_Bytes  of char list
  | Data_Ascii  of string
  | Data_Asciz  of string
  | Data_Word   of int32 list
  | Data_GLabel of int * string
  | Data_Label  of string

(* Program ------------------------------------------------------------------ *)

type program = {
    data: data_line list;
    text: text_line list;
  }

