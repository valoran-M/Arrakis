(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Insts
open Iutils

type t = { funct3: int; rs1: int; imm: int32; rd: int }

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

(* code and decode ---------------------------------------------------------- *)

let decode code =
  let (>>) = Int.shift_right_logical in
  let (&&) x y = Int32.to_int (x & y) in
  {
    funct3 = (func3_mask && code) >> 12;
    rs1 = (rs1_mask && code) >> 15;
    imm = Int32.shift_right_logical (Int32.logand imm12_mask code) 20;
    rd  = (rd_mask && code) >> 7;
  }

let code instruction rd rs1 imm =
  if 4096l < (imm & 0b11111111111l)
  then raise (Ierror.Instruction_error (Interval_imm (imm, -2048l, 2027l)))
  else (
    let (<<) = Int32.shift_left in
    let (||) = Int32.logor in
    let (opcode, funct3, _) = Hashtbl.find instructions instruction in
    (imm << 20) || (rs1 << 15) || (funct3 << 12) || (rd << 7) || opcode
  )

