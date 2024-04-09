(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

type imm =
  | Label of string
  | Imm   of int32

(* Real instructions -------------------------------------------------------- *)

type r_instruction =
  (* RV32I *)
  | ADD | SUB
  | XOR | OR  | AND
  | SLL | SRL | SRA
  | SLT | SLTU
  (* RV32M *)
  | MUL    | MULH
  | MULHSU | MULHU
  | DIV    | DIVU
  | REM    | REMU

type i_instruction =
  (* arit immediate *)
  | ADDI
  | XORI | ORI  | ANDI
  | SLLI | SRLI | SARI
  | SLTI | SLTIU
  (* load *)
  | LB  | LH  | LW
  | LBU | LHU
  (* other *)
  | JALR
  | ECALL| EBREAK

type s_instruction =
  (* store *)
  | SB | SH | SW

type b_instruction =
  (* branch *)
  | BEQ  | BNE
  | BLT  | BGE
  | BLTU | BGEU

type u_instruction =
  | LUI | AUIPC

type j_instruction =
  | JAL

(* Pseudo instructions ------------------------------------------------------ *)

type two_reg =
  | MV   | NOT  | NEG
  | SEQZ | SNEZ | SLTZ | SGTZ

type reg_offset =
  | BEQZ | BNEZ | BLEZ
  | BGEZ | BLTZ | BGTZ

type reg_reg_offset =
  | BGT  | BLE
  | BGTU | BLEU

type pseudo_inst =
  | NOP
  | LI    of int32 * imm
  | LA    of int32 * imm
  | J     of imm
  | JALP  of imm
  | JR    of int32
  | JALRP  of int32
  | RET
  | CALL  of imm
  | TAIL  of imm
  | LGlob of int32 * imm * i_instruction
  | SGlob of int32 * imm * int32 * s_instruction
  | Two_Regs    of two_reg    * int32 * int32
  | Regs_Offset of reg_offset * int32 * imm
  | Regs_Regs_Offset of reg_reg_offset * int32 * int32 * imm

(* Code --------------------------------------------------------------------- *)

type basics_inst =
                      (* rd      rs1     rs2 *)
  | R of r_instruction * int32 * int32 * int32
                      (* rd      rs1   *)
  | I of i_instruction * int32 * int32 * imm
                      (* rs2     rs1   *)
  | S of s_instruction * int32 * int32 * imm
                      (* rs1     rs2   *)
  | B of b_instruction * int32 * int32 * imm
                      (* rd    *)
  | U of u_instruction * int32 * imm
                      (* rd    *)
  | J of j_instruction * int32 * imm

