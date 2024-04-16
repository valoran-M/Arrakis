(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open History
open Arch
open Insts
open Utils
open Global_utils.Integer

(* Instruction format :
   31                     20 19      15 14  12 11         7 6            0
  +-----------------------------------------------------------------------+
  | imm[11:0]               | rs1      |funct3| rd         | opcode       | I
  +-----------------------------------------------------------------------+
*)

type t = { fc3: int; rs1: int; imm: int32; rdt: int }

exception Syscall


let instructions =
  [
(*  inst    Opcode        funct3  str      *)
    ADDI,   (0b0010011l,  0x0l,   "addi"   );
    XORI,   (0b0010011l,  0x4l,   "xori"   );
    ORI,    (0b0010011l,  0x6l,   "ori"    );
    ANDI,   (0b0010011l,  0x7l,   "andi"   );
    SLLI,   (0b0010011l,  0x1l,   "slli"   );
    SRLI,   (0b0010011l,  0x5l,   "srli"   );
    SARI,   (0b0010011l,  0x5l,   "sari"   );
    SLTI,   (0b0010011l,  0x2l,   "slti"   );
    SLTIU,  (0b0010011l,  0x3l,   "sltiu"  );
    LB,     (0b0000011l,  0x0l,   "lb"     );
    LH,     (0b0000011l,  0x1l,   "lh"     );
    LW,     (0b0000011l,  0x2l,   "lw"     );
    LBU,    (0b0000011l,  0x4l,   "lbu"    );
    LHU,    (0b0000011l,  0x5l,   "lhu"    );
    JALR,   (0b1100111l,  0x0l,   "jalr"   );
    ECALL,  (0b1110011l,  0x0l,   "ecall"  );
    (* EBREAK, (0b1110011l,  0x0l,   "ebreak" ); *)
  ]

let instructions, str_table = create_tables instructions (fun (_, _, v) -> v)

(* Code and decode ---------------------------------------------------------- *)

let code instruction rd rs1 imm =
  if 4096l < (imm & 0b11111111111l)
  then raise (Error.Instruction_error (Interval_imm (imm, -2048l, 2027l)))
  else (
    let (opcode, funct3, _) = Hashtbl.find instructions instruction in
    (imm << 20l) || (rs1 << 15l) || (funct3 << 12l) || (rd << 7l) || opcode
  )

let decode code =
  let (>>) = Int.shift_right_logical in
  let (&&) x y = Int32.to_int (x & y) in
  let imm11_0 = (i12_mask & code) >>> 20l in
  { fc3 = (fc3_mask && code) >> 12;
    rs1 = (rs1_mask && code) >> 15;
    imm = sign_extended imm11_0 12;
    rdt = (rdt_mask && code) >> 07; }

(* Execution ---------------------------------------------------------------- *)

let execute_arith instruction rs1 =
  let imm = instruction.imm in
  (* if imm[5:11] = 0x20 or 0x00 for shift *)
  let arith = Int32.shift_right_logical imm 5 = 0x20l in
  let logic = Int32.shift_right_logical imm 5 = 0x00l in
  match instruction.fc3 with
  | 0x0 -> rs1 +  imm                           (* ADDI  *)
  | 0x4 -> rs1 ^  imm                           (* XORI  *)
  | 0x6 -> rs1 || imm                           (* ORI   *)
  | 0x7 -> rs1 &  imm                           (* ANDI  *)
  | 0x1 when logic -> rs1 <<  imm               (* SLLI  *)
  | 0x5 when logic -> rs1 >>> imm               (* SRLI  *)
  | 0x5 when arith -> rs1 >>  (imm &  0b11111l) (* SRAI  *)
  | 0x2 -> if rs1 < imm then 1l else 0l         (* SLTI  *)
  | 0x3 -> if rs1 <.imm then 1l else 0l         (* SLTIU *)
  | _ -> Error.i_invalid_arith instruction.fc3 instruction.imm

let execute_load instruction rs1 memory =
  let imm = instruction.imm in
  let addr = rs1 + imm in
  match instruction.fc3 with
  | 0x0 -> sign_extended (Memory.get_byte  memory addr) 8  (* LB  *)
  | 0x1 -> sign_extended (Memory.get_int16 memory addr) 16 (* LH  *)
  | 0x2 -> sign_extended (Memory.get_int32 memory addr) 32 (* LW  *)
  | 0x4 -> Memory.get_byte  memory addr                    (* LBU *)
  | 0x5 -> Memory.get_int16 memory addr                    (* LHU *)
  | _ -> Error.i_invalid_load instruction.fc3

let execute_jmp instruction rs1 pc =
  let imm = instruction.imm in
  match instruction.fc3 with
  | 0x0 -> (pc + 4l, rs1 + imm)                           (* JALR *)
  | _   -> Error.i_invalid_load instruction.fc3

let execute opcode instruction (arch : Riscv.t) =
  let open Cpu in
  let cpu = arch.cpu in
  let ins = decode instruction in
  let rs1 = Regs.get cpu.regs ins.rs1 in
  let lst = Regs.get cpu.regs ins.rdt in
  match opcode with
  | 0b0010011l ->
      let res = execute_arith ins rs1 in
      Regs.set cpu.regs ins.rdt res;
      next_pc cpu;
      Change_Register (ins.rdt, lst)
  | 0b0000011l ->
      let res = execute_load ins rs1 arch.memory in
      Regs.set cpu.regs ins.rdt res;
      next_pc cpu; Change_Register (ins.rdt, lst)
  | 0b1100111l ->
      let (rd, pc) = execute_jmp ins rs1 (get_pc cpu) in
      set_reg cpu ins.rdt rd; set_pc  cpu pc;
      Change_Register (ins.rdt, lst)
  | 0b1110011l ->
      if Int32.equal ins.imm 0x0l
      then (next_pc cpu; raise Syscall)
      else Error.i_invalid ins.fc3 opcode ins.imm
  | _ -> assert false     (* opcode in { 0010011; 0000011; 0000011; 1110011 } *)

