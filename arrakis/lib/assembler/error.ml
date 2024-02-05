(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

type t =
  | Unknown_Label  of string
                   (* val     min     max     *)
  | Interval_imm   of int32 * int32 * int32
  | Parsing_error  of string
  | Lexing_error   of string

exception Assembler_error of int * t

let raise_unclosed line =
  raise (Assembler_error (line, Lexing_error "\" unclosed"))
