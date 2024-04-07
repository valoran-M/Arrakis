(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions

exception Invalid_instruction

let ( + ) = Int32.add
let ( * ) = Int32.mul

(* Instructions ------------------------------------------------------------- *)

let reg_to_str reg = "x" ^ (Int.to_string reg)

let r_to_string = Hashtbl.create 18
let i_to_string = Hashtbl.create 18
let s_to_string = Hashtbl.create 3
let b_to_string = Hashtbl.create 6
let u_to_string = Hashtbl.create 2
let j_to_string = Hashtbl.create 1

let () =
  (* R *)
  Hashtbl.iter (fun _ (_, funct3, funct7, str) ->
      Hashtbl.add r_to_string (Int32.to_int funct3, Int32.to_int funct7) str)
    R.instructions;
  (* I *)
  Hashtbl.iter (fun _ (opcode, funct3, str) ->
      Hashtbl.add i_to_string (opcode, Int32.to_int funct3) str)
    I.instructions;
  (* S *)
  Hashtbl.iter (fun _ (_, funct3, str) ->
      Hashtbl.add s_to_string (Int32.to_int funct3) str)
    S.instructions;
  (* B *)
  Hashtbl.iter (fun _ (_, funct3, str) ->
      Hashtbl.add b_to_string (Int32.to_int funct3) str)
    B.instructions;
  (* U *)
  Hashtbl.iter (fun _ (opcode, str) ->
      Hashtbl.add u_to_string opcode str)
    U.instructions;
  (* J *)
  Hashtbl.iter (fun _ (opcode, str) ->
      Hashtbl.add j_to_string opcode str)
    J.instructions

let print_code _arch code =
  let opcode = Int32.logand 0b1111111l code in
  match opcode with
  (* R *)
  | 0b0110011l ->
    let inst = R.decode code in
    Format.sprintf "%s %s, %s, %s"
      (Hashtbl.find r_to_string (inst.fc3, inst.fc7))
      (reg_to_str inst.rdt) (reg_to_str inst.rs1) (reg_to_str inst.rs2)
  (* I *)
  | 0b0010011l | 0b1100111l | 0b1110011l ->
    let inst = I.decode code in
    Format.sprintf "%s %s, %s, %ld"
      (Hashtbl.find i_to_string (opcode, inst.fc3))
      (reg_to_str inst.rdt) (reg_to_str inst.rs1)
      inst.imm
  | 0b0000011l ->
    let inst = I.decode code in
    Format.sprintf "%s %s, 0x%lx(%s)"
      (Hashtbl.find i_to_string (opcode, inst.fc3))
      (reg_to_str inst.rdt)
      inst.imm
      (reg_to_str inst.rs1)
  (* S *)
  | 0b0100011l ->
    let inst = S.decode code in
    Format.sprintf "%s %s, 0x%lx(%s)"
      (Hashtbl.find s_to_string inst.fc3)
      (reg_to_str inst.rs2)
      inst.imm
      (reg_to_str inst.rs1)
  (* B *)
  | 0b1100011l  ->
    let inst = B.decode code in
    Format.sprintf "%s %s, %s, 0x%lx"
      (Hashtbl.find b_to_string inst.fc3)
      (reg_to_str inst.rs1) (reg_to_str inst.rs2)
      inst.imm
  (* U *)
  | 0b0110111l | 0b0010111l  ->
    let inst = U.decode code in
    Format.sprintf "%s %s, %ld" (Hashtbl.find u_to_string opcode)
      (reg_to_str inst.rdt) (Int32.shift_right_logical inst.imm 12)
  (* J *)
  | 0b1101111l   ->
    let inst = J.decode code in
    let imm  =
      if inst.imm < 0l then Format.sprintf "-0x%lx" (Int32.neg inst.imm)
                       else Format.sprintf " 0x%lx" inst.imm
    in
    Format.sprintf "%s %s" (Hashtbl.find j_to_string opcode) imm
  | _ -> raise Invalid_instruction

