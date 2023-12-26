(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(*
  The assembly code is transformed in byte code.

  parser ->
  get_label_address ->
  remove pseudo instruction ->
  write instruction in memory
*)

open Utils
open Error
open Program

let translate (instruction : instruction) mem addr line labels =
  let imm_to_int32 = Utils.imm_to_int32 labels in

  match instruction with
  | R (inst, rd, rs1, rs2) -> Inst_R.write_in_memory mem addr inst rd rs1 rs2
  | I (inst, rd, rs1, imm) ->
    let imm = imm_to_int32 line addr imm in
    Inst_I.write_in_memory mem addr inst rd rs1 imm line
  | S (inst, rs2, rs1, imm) ->
    let imm = imm_to_int32 line addr imm in
    Inst_S.write_in_memory mem addr inst rs2 rs1 imm
  | B (inst, rs1, rs2, imm) ->
    let imm = imm_to_int32 line addr imm in
    Inst_B.write_in_memory mem addr inst rs1 rs2 imm
  | U (inst, rd, imm) ->
    let imm = imm_to_int32 line addr imm in
    Inst_U.write_in_memory mem addr inst rd imm
  | J (inst, rd, imm) ->
    let imm = imm_to_int32 line addr imm in
    Inst_J.write_in_memory mem addr inst rd imm

let loop_prog mem debug labels addr prog  =
  match prog with
  | Prog_Instr (l, s, inst) ->
    Debug.add_addr_to_line debug addr l s;
    Debug.add_line_to_addr debug l addr;
    translate inst mem addr l labels;
    addr + 4l
  | Prog_Label _  -> addr
  (* No more pseudo instructions after remove_pseudo *)
  | Prog_Pseudo _ -> assert false
  | Prog_GLabel (line, label) -> Label.made_global labels label line; addr

let loop_memory mem labels addr (prog : memory_line) =
  match prog with
  | Mem_Label _              -> addr
  | Mem_GLabel (line, label) -> Label.made_global labels label line; addr
  | Mem_Value v              -> Simulator.Memory.set_int32 mem addr v; addr + 4l
  | Mem_Bytes lb ->
    List.fold_left (fun addr v ->
      Simulator.Memory.set_byte mem addr (Simulator.Utils.char_to_int32 v);
      addr + 1l)
    addr lb
  | Mem_Asciz s ->
    let addr =
      String.fold_left (fun addr v ->
        Simulator.Memory.set_byte mem addr (Simulator.Utils.char_to_int32 v);
        addr + 1l)
      addr s
    in
    Simulator.Memory.set_byte mem addr 0l; (addr + 1l)
  | Mem_Word words ->
    List.fold_left (fun addr v ->
      Simulator.Memory.set_int32 mem addr v; addr + 4l)
      addr words
  | Mem_Zero nz ->
    Simulator.Memory.set_32b_zero mem addr nz;
    addr + 4l * nz

let assembly code =
    let open Simulator.Segment in
  try
    let mem  = Simulator.Memory.make () in
    let debug = Debug.generate_debug () in

    let prog = Parser.program Lexer.token code in

    let labels = Label.get_label_address prog in
    ignore (List.fold_left (loop_memory mem labels)
      static_being prog.memory);
    let prog = Transform_pseudo.remove_pseudo prog.program labels in
    ignore (List.fold_left (loop_prog mem debug labels)
      text_begin prog);

    (mem, labels, debug)
  with Parser.Error ->
    let pe = Parsing_error (Lexing.lexeme code) in
    raise (Assembler_error (!Lexer.line, pe))

