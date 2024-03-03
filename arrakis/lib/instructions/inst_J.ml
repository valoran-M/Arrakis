(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Insts
open Iutils

let instructions =
    [
    (*  inst   Opcode      str   *)
      JAL,   (0b1101111l, "jal" );
    ]

let instructions, str_table = create_tables instructions (fun (_, v) -> v)

(* code and decode ---------------------------------------------------------- *)

let code instruction rd imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, _) = Hashtbl.find instructions instruction in
  let imm20    = get_interval imm 20 20 in
  let imm19_12 = get_interval imm 19 12 in
  let imm11    = get_interval imm 11 11 in
  let imm10_1  = get_interval imm 10 1  in
  (imm20 << 31)    || (imm10_1 << 21) || (imm11 << 20) ||
  (imm19_12 << 12) || (rd << 7) || opcode

