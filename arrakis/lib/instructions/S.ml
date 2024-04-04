(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Insts
open Utils
open Arch
open Global_utils.Integer

(* Instruction format :
   31          25 24      20 19      15 14  12 11         7 6            0
  +-----------------------------------------------------------------------+
  | imm[11:5]    | rs2      | rs1      |funct3| imm[4:0]   | opcode       | S
  +-----------------------------------------------------------------------+
*)

type t = { fc3: int; rs1: int; rs2: int; imm: int32; }

let instructions =
  [
    (*  inst Opcode       funct3 str  *)
    SB,  (0b0100011l,  0x0l,   "sb" );
    SH,  (0b0100011l,  0x1l,   "sh" );
    SW,  (0b0100011l,  0x2l,   "sw" );
  ]

let instructions, str_table = create_tables instructions (fun (_, _, v) -> v)

(* Code and decode ---------------------------------------------------------- *)

let code instruction rs2 rs1 imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor  in
  let (opcode, funct3, _) = Hashtbl.find instructions instruction in
  let imm11_5 = get_interval imm 11 5 in
  let imm04_0 = get_interval imm 04 0 in
  (imm11_5 << 25) || (rs2 << 20) || (funct3 << 12) ||
  (imm04_0 << 07) || (rs1 << 15) || opcode

let decode code =
  let (>>) = Int.shift_right_logical  in
  let (&&) x y = Int32.to_int (x & y) in
  { fc3 = (code && fc3_mask) >> 12;
    rs1 = (code && rs1_mask) >> 15;
    rs2 = (code && rs2_mask) >> 20;
    imm = sign_extended (((code & fc7_mask) >>> 20l) ||
                         ((code & rdt_mask) >>> 07l)) 12; }

(* Exectuion ---------------------------------------------------------------- *)

let execute_st instruction rs1 rs2 memory =
  let addr = rs1 + instruction.imm in
  match instruction.fc3 with
  | 0x0 -> (* SB *)
    let last_value = Memory.get_byte memory addr in
    Memory.set_byte memory addr (rs2 & 0b11111111l);
    (8, addr, last_value)
  | 0x1 ->
    (* SH *)
    let last_value = Memory.get_int16 memory addr in
    Memory.set_int16 memory addr (rs2 & 0b1111111111111111l);
    (16, addr, last_value)
  | 0x2 ->
    (* SW *)
    let last_value = Memory.get_int32 memory addr in
    Memory.set_int32 memory addr rs2;
    (32, addr, last_value)
  | _ -> Error.s_invalid instruction.fc3

let execute _opcode instruction (arch : Riscv.t) =
  let open Cpu in
  let cpu = arch.cpu in
  let ins = decode instruction in
  let rs1 = Regs.get cpu.regs ins.rs1 in
  let rs2 = Regs.get cpu.regs ins.rs2 in
  let (length, addr, lst) = execute_st ins rs1 rs2 arch.memory in
  next_pc cpu; History.create_write_mem length addr lst

