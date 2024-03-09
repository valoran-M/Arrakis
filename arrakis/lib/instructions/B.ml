(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Insts
open Utils
open Global_utils.Integer

(* Instruction format :
   31          25 24      20 19      15 14  12 11         7 6            0
  +-----------------------------------------------------------------------+
  | imm[12|10:5] | rs2      | rs1      |funct3| imm[4:1|11]| opcode       | B
  +-----------------------------------------------------------------------+
*)

type t = { funct3: int; rs1: int; rs2: int; imm: int32; }

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

let decode code =
  (* Imm interval *)
  let imm_7 = (code & func7_mask) >> 25l in
  let imm_5 = (code & rd_mask) >> 7l in
  (* Imm's bits *)
  let imm12   = (imm_7 & 0b1000000l) << 6l in
  let imm10_5 = (imm_7 & 0b0111111l) << 5l in
  let imm11   = (imm_5 & 0b00001l) << 11l  in
  let imm4_1  = imm_5 & 0b11110l           in
  let imm = sign_extended (imm12 || imm11 || imm10_5 || imm4_1) 13 in

  let (>>) = Int.shift_right_logical in
  let (&&) x y = Int32.to_int (x & y) in
  {
    funct3 = (code && func3_mask) >> 12;
    rs1 = (code && rs1_mask) >> 15;
    rs2 = (code && rs2_mask) >> 20;
    imm = imm;
  }

let code instruction rs1 rs2 imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, funct3, _) = Hashtbl.find instructions instruction in
  let imm12   = get_interval imm 12 12 in
  let imm11   = get_interval imm 11 11 in
  let imm10_5 = get_interval imm 10 5  in
  let imm4_1  = get_interval imm 4  1  in
  (imm12 << 31) || (imm10_5 << 25) || (rs2    << 20) ||
  (rs1   << 15) || (funct3  << 12) || (imm4_1 <<  8) ||
  (imm11 <<  7) || opcode

(* Exectuion ---------------------------------------------------------------- *)

let execute instruction rs1 rs2 =
  let test f x y = if f x y then instruction.imm else 4l in
  match instruction.funct3 with
  | 0x0 -> test (=)   rs1 rs2 (* BEQ  *)
  | 0x1 -> test (<>)  rs1 rs2 (* BNE  *)
  | 0x4 -> test (<)   rs1 rs2 (* BLT  *)
  | 0x5 -> test (>=)  rs1 rs2 (* BGE  *)
  | 0x6 -> test (<.)  rs1 rs2 (* BLTU *)
  | 0x7 -> test (>=.) rs1 rs2 (* BGEU *)
  | _ -> Error.b_invalid instruction.funct3

