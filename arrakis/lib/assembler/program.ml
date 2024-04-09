(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions.Insts

type program_line =
                (* line nb, original code *)
  | Prog_Pseudo of int      * string      * pseudo_instruction
  | Prog_Instr  of int      * string      * instruction
  | Prog_GLabel of int      * string
  | Prog_Label  of string

(* Memory ------------------------------------------------------------------- *)

type memory_line =
  | Mem_Zero   of int32
  | Mem_Bytes  of char list
  | Mem_Ascii  of string
  | Mem_Asciz  of string
  | Mem_Word   of int32 list
  | Mem_GLabel of int * string
  | Mem_Label  of string

(* Program ------------------------------------------------------------------ *)

type program = {
    memory  : memory_line  list;
    program : program_line list;
  }

