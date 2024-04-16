(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Arch
open History
open Insts
open Utils
open Gutils.Integer

(* Instruction format :
   31          25 24      20 19      15 14  12 11         7 6            0
  +-----------------------------------------------------------------------+
  | imm[12|10:5] | rs2      | rs1      |funct3| imm[4:1|11]| opcode       | B
  +-----------------------------------------------------------------------+
*)

type t = { fc3: int; rs1: int; rs2: int; imm: int32; }

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
  let imm_7 = (code & fc7_mask) >> 25l in
  let imm_5 = (code & rdt_mask) >> 07l in
  (* Imm's bits *)
  let imm12_12 = (imm_7 & 0b1000000l) << 06l in
  let imm10_05 = (imm_7 & 0b0111111l) << 05l in
  let imm11_11 = (imm_5 & 0b0000001l) << 11l in
  let imm04_01 = (imm_5 & 0b0011110l)        in
  let imm = sign_extended (imm12_12 || imm11_11 || imm10_05 || imm04_01) 13 in

  let (>>) = Int.shift_right_logical in
  let (&&) x y = Int32.to_int (x & y) in
  { fc3 = (code && fc3_mask) >> 12;
    rs1 = (code && rs1_mask) >> 15;
    rs2 = (code && rs2_mask) >> 20;
    imm; }

let code instruction rs1 rs2 imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor      in
  let (opcode, funct3, _) = Hashtbl.find instructions instruction in
  let imm12_12 = get_interval imm 12 12 in
  let imm11_11 = get_interval imm 11 11 in
  let imm10_05 = get_interval imm 10 05 in
  let imm04_01 = get_interval imm 04 01 in
  (imm12_12 << 31) || (imm10_05 << 25) || (rs2 << 20) || (funct3  << 12) ||
  (imm04_01 <<  8) || (imm11_11 <<  7) || (rs1 << 15) || opcode

(* Execution ---------------------------------------------------------------- *)

let execute_tests instruction rs1 rs2 =
  let test f x y = if f x y then instruction.imm else 4l in
  match instruction.fc3 with
  | 0x0 -> test (=  ) rs1 rs2 (* BEQ  *)
  | 0x1 -> test (<> ) rs1 rs2 (* BNE  *)
  | 0x4 -> test (<  ) rs1 rs2 (* BLT  *)
  | 0x5 -> test (>= ) rs1 rs2 (* BGE  *)
  | 0x6 -> test (<. ) rs1 rs2 (* BLTU *)
  | 0x7 -> test (>=.) rs1 rs2 (* BGEU *)
  | _ -> Error.b_invalid instruction.fc3

let execute _opcode instruction (arch : Riscv.t) =
  let open Cpu in
  let cpu = arch.cpu in
  let ins = decode instruction in
  let rs1 = Regs.get cpu.regs ins.rs1 in
  let rs2 = Regs.get cpu.regs ins.rs2 in
  let imm = execute_tests ins rs1 rs2 in
  add_pc cpu imm; Change_Nothing

