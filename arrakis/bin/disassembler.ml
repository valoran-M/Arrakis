open Simulator

exception Invalide_instruction

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
    Assembler.Inst_R.r_instructions;
  (* I *)
  Hashtbl.iter (fun _ (opcode, funct3, str) ->
      Hashtbl.add i_to_string (opcode, Int32.to_int funct3) str)
    Assembler.Inst_I.i_instructions;
  (* S *)
  Hashtbl.iter (fun _ (_, funct3, str) ->
      Hashtbl.add s_to_string (Int32.to_int funct3) str)
    Assembler.Inst_S.s_instructions;
  (* B *)
  Hashtbl.iter (fun _ (_, funct3, str) ->
      Hashtbl.add b_to_string (Int32.to_int funct3) str)
    Assembler.Inst_B.b_instructions;
  (* U *)
  Hashtbl.iter (fun _ (opcode, str) ->
      Hashtbl.add u_to_string opcode str)
    Assembler.Inst_U.u_instructions;
  (* J *)
  Hashtbl.iter (fun _ (opcode, str) ->
      Hashtbl.add j_to_string opcode str)
    Assembler.Inst_J.j_instructions

let print_code _arch code =
  let opcode = Int32.logand 0b1111111l code in
  match opcode with
  (* R type *)
  | 0b0110011l ->
    let inst = Instructions.R_type.decode code in
    Printf.sprintf "%s %s, %s, %s"
      (Hashtbl.find r_to_string (inst.funct3, inst.funct7))
      (reg_to_str inst.rd) (reg_to_str inst.rs1) (reg_to_str inst.rs2)
  (* I type *)
  | 0b0010011l | 0b1100111l | 0b1110011l ->
    let inst = Instructions.I_type.decode code in
    Printf.sprintf "%s %s, %s, %d"
      (Hashtbl.find i_to_string (opcode, inst.funct3))
      (reg_to_str inst.rd) (reg_to_str inst.rs1)
      (Int32.to_int (Simulator.Utils.sign_extended inst.imm 12))
  | 0b0000011l ->
    let inst = Instructions.I_type.decode code in
    Printf.sprintf "%s %s, %d(%s)"
      (Hashtbl.find i_to_string (opcode, inst.funct3))
      (reg_to_str inst.rd)
      (Int32.to_int (Simulator.Utils.sign_extended inst.imm 12))
      (reg_to_str inst.rs1)
  (* S type *)
  | 0b0100011l ->
    let inst = Instructions.S_type.decode code in
    Printf.sprintf "%s %s, %d(%s)"
      (Hashtbl.find s_to_string inst.funct3)
      (reg_to_str inst.rs2)
      (Int32.to_int (Simulator.Utils.sign_extended inst.imm 12))
      (reg_to_str inst.rs1)
  (* B type *)
  | 0b1100011l  ->
    let inst = Instructions.B_type.decode code in
    Printf.sprintf "%s %s, %s, 0x%x"
      (Hashtbl.find b_to_string inst.funct3)
      (reg_to_str inst.rs1) (reg_to_str inst.rs2) (Int32.to_int inst.imm)
  (* U type *)
  | 0b0110111l | 0b0010111l  ->
    let inst = Instructions.U_type.decode code in
    Printf.sprintf "%s %d" (Hashtbl.find u_to_string opcode)
      (Int32.to_int inst.imm_shift)
  (* J type *)
  | 0b1101111l   ->
    let inst = Instructions.J_type.decode code in
    Printf.sprintf " %s%d" (Hashtbl.find j_to_string opcode)
        (Int32.to_int inst.imm)
  | _ -> raise Invalide_instruction

