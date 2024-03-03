(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Insts
open Utils

let instructions =
  [
(*  inst   Opcode       funct3  str    *)
    BEQ,   (0b1100011l, 0x0l,   "beq"  );
    BNE,   (0b1100011l, 0x1l,   "bne"  );
    BLT,   (0b1100011l, 0x4l,   "blt"  );
    BGE,   (0b1100011l, 0x5l,   "bge"  );
    BLTU,  (0b1100011l, 0x6l,   "bltu" );
    BGEU,  (0b1100011l, 0x7l,   "bgeu" );
  ]

let instructions, str_table = create_tables instructions (fun (_, _, v) -> v)

(* Code and decode ---------------------------------------------------------- *)

let code instruction rs1 rs2 imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, funct3, _) = Hashtbl.find instructions instruction in
  let imm12   = get_interval imm 12 12 in
  let imm11   = get_interval imm 11 11 in
  let imm10_5 = get_interval imm 10 5  in
  let imm4_1  = get_interval imm 4  1  in
  (imm12 << 31) || (imm10_5 << 25) || (rs2 << 20)   ||
  (rs1 << 15)   || (funct3 << 12)  || (imm4_1 << 8) ||
  (imm11 << 7)  || opcode

