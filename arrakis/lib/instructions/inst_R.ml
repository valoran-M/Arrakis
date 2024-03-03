(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Iutils
open Insts

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

let code instruction rd rs1 rs2 =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, funct3, funct7, _) = Hashtbl.find instructions instruction in
  (funct7 << 25) || (rs2 << 20) || (rs1 << 15) || (funct3 << 12) ||
  (rd << 7) || opcode

