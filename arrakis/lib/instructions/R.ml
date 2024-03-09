(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Insts
open Utils

(* Instruction format :
   31          25 24      20 19      15 14  12 11         7 6            0
  +-----------------------------------------------------------------------+
  | funct7       | rs2      | rs1      |funct3| rd         | opcode       |
  +-----------------------------------------------------------------------+
*)

type t = { funct7 : int; funct3: int; rs1: int; rs2: int; rd: int; }

let instructions =
    [
  (*  inst    Opcode       funct3 funct7 str      *)
      ADD,    (0b0110011l, 0x0l,  0x00l, "add"    );
      SUB,    (0b0110011l, 0x0l,  0x20l, "sub"    );
      XOR,    (0b0110011l, 0x4l,  0x00l, "xor"    );
      OR,     (0b0110011l, 0x6l,  0x00l, "or"     );
      AND,    (0b0110011l, 0x7l,  0x00l, "and"    );
      SLL,    (0b0110011l, 0x1l,  0x00l, "sll"    );
      SRL,    (0b0110011l, 0x5l,  0x00l, "srl"    );
      SRA,    (0b0110011l, 0x5l,  0x20l, "sra"    );
      SLT,    (0b0110011l, 0x2l,  0x00l, "slt"    );
      SLTU,   (0b0110101l, 0x3l,  0x00l, "sltu"   );
  (*  RV32M                                       *)
      MUL,    (0b0110011l, 0x0l,  0x01l, "mul"    );
      MULH,   (0b0110011l, 0x1l,  0x01l, "mulh"   );
      MULHSU, (0b0110011l, 0x2l,  0x01l, "mulhsu" );
      MULHU,  (0b0110011l, 0x3l,  0x01l, "mulhu"  );
      DIV,    (0b0110011l, 0x4l,  0x01l, "div"    );
      DIVU,   (0b0110011l, 0x5l,  0x01l, "divu"   );
      REM,    (0b0110011l, 0x6l,  0x01l, "rem"    );
      REMU,   (0b0110011l, 0x7l,  0x01l, "remu"   );
    ]

let instructions, str_table = create_tables instructions (fun (_, _, _, v) -> v)

(* code and decode ---------------------------------------------------------- *)

let decode code =
    let (>>) = Int.shift_right_logical in
    let (&&) x y = Int32.to_int (x &  y) in
    {
      funct7 = (func7_mask && code) >> 25;
      funct3 = (func3_mask && code) >> 12;
      rs1 = (rs1_mask && code) >> 15;
      rs2 = (rs2_mask && code) >> 20;
      rd = (rd_mask && code) >> 7;
    }

let code instruction rd rs1 rs2 =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, funct3, funct7, _) = Hashtbl.find instructions instruction in
  (funct7 << 25) || (rs2 << 20) || (rs1 << 15) || (funct3 << 12) ||
  (rd     <<  7) || opcode

(* Exectuion ---------------------------------------------------------------- *)

let execute instruction rs1 rs2 =
  match instruction.funct3, instruction.funct7 with
  (* RV32I *)
  | 0x0, 0x00 -> rs1 +  rs2                   (* ADD    *)
  | 0x0, 0x20 -> rs1 -  rs2                   (* SUB    *)
  | 0x4, 0x00 -> rs1 ^  rs2                   (* XOR    *)
  | 0x6, 0x00 -> rs1 || rs2                   (* OR     *)
  | 0x7, 0x00 -> rs1 &  rs2                   (* AND    *)
  | 0x1, 0x00 -> rs1 << rs2                   (* SLL    *)
  | 0x5, 0x00 -> rs1 >>> rs2                  (* SRL    *)
  | 0x5, 0x20 -> rs1 >>  rs2                  (* SRA    *)
  | 0x2, 0x00 -> if rs1 < rs2 then 1l else 0l (* SLT    *)
  | 0x3, 0x00 -> if rs1 <.rs2 then 1l else 0l (* SLTU   *)
  (* RV32M *)
  | 0x0, 0x01 -> rs1 * rs2                    (* MUL    *)
  | 0x1, 0x01 -> mulh rs1 rs2                 (* MULH   *)
  | 0x2, 0x01 -> mulhsu rs1 rs2               (* MULHSU *)
  | 0x3, 0X01 -> mulhu rs1 rs2                (* MULHU  *)
  | 0x4, 0x01 -> rs1 / rs2                    (* DIV    *)
  | 0x5, 0x01 -> rs1 /. rs2                   (* DIVU   *)
  | 0x6, 0x01 -> rs1 % rs2                    (* REM    *)
  | 0x7, 0x01 -> rs1 %. rs2                   (* REMU   *)
  | _, _ -> Error.r_invalid instruction.funct3 instruction.funct7

