(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open History
open Insts
open Utils
open Global_utils.Integer

(* Instruction format :
   31                                       12 11         7 6            0
  +-----------------------------------------------------------------------+
  | imm[20|10:1|11|19:12]                     | rd         | opcode       | J
  +-----------------------------------------------------------------------+
*)

type t = { rd: int; imm : int32; }

let instructions =
    [
    (*  inst   Opcode      str   *)
      JAL,   (0b1101111l, "jal" );
    ]

let instructions, str_table = create_tables instructions (fun (_, v) -> v)

(* Code and decode ---------------------------------------------------------- *)

let code instruction rd imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, _) = Hashtbl.find instructions instruction in
  let imm20    = get_interval imm 20 20 in
  let imm19_12 = get_interval imm 19 12 in
  let imm11    = get_interval imm 11 11 in
  let imm10_1  = get_interval imm 10 1  in
  (imm20    << 31) || (imm10_1 << 21) || (imm11 << 20) ||
  (imm19_12 << 12) || (rd      <<  7) || opcode

let decode code =
  (* Imm interval *)
  let imm_31_20 = (code & (func7_mask || rs2_mask)) >> 20l in
  (* Imm's bits *)
  let imm19_12 = code & 0b11111111000000000000l in
  let imm11 = (imm_31_20 & 0b1l) << 11l         in
  let imm10_1 = (imm_31_20 & 0b11111111110l)    in
  let imm20 = (imm_31_20 & 0b100000000000l)     in
  let imm = sign_extended (imm20 || imm19_12 || imm11 || imm10_1) 21 in

  let (>>) = Int.shift_right_logical in
  let (&&) x y = Int32.to_int (x & y) in
  {
    rd = (code && rd_mask) >> 7;
    imm = imm;
  }

(* Exectuion ---------------------------------------------------------------- *)

let execute _opcode instruction cpu _memory =
  let open Arch.Cpu in
  let ins = decode instruction in
  let lst = get_reg cpu ins.rd in
  (* JAL *)
  set_reg cpu ins.rd (Int32.add (get_pc cpu) 4l);
  add_pc cpu ins.imm; Change_Register (ins.rd, lst)

