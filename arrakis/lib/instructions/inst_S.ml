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
    (*  inst Opcode       funct3 str  *)
    SB,  (0b0100011l,  0x0l,   "sb" );
    SH,  (0b0100011l,  0x1l,   "sh" );
    SW,  (0b0100011l,  0x2l,   "sw" );
  ]

let instructions, str_table = create_tables instructions (fun (_, _, v) -> v)

let code instruction rs2 rs1 imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor  in
  let (opcode, funct3, _) = Hashtbl.find instructions instruction in
  let imm11_5 = get_interval imm 11 5 in
  let imm4_0  = get_interval imm  4 0 in
  (imm11_5 << 25) || (rs2 << 20)   || (rs1 << 15) || (funct3 << 12) ||
  (imm4_0 << 7) || opcode

