(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(*
  Translation of assembly code to byte code.

  parser ->
  get_label_address ->
  remove pseudo instruction ->
  write instruction in memory
*)

open Utils
open Sim_utils.Integer
open Error
open Instructions.Insts
open Program
open Arch

let translate (instruction : instruction) addr line labels =
  let imm_to_int32 = imm_to_int32 labels in
  match instruction with
  | R (inst, rd, rs1, rs2) -> Instructions.R.code inst rd rs1 rs2
  | I (inst, rd, rs1, imm) ->
    let imm = imm_to_int32 line addr imm in
    Instructions.I.code inst rd rs1 imm
  | S (inst, rs2, rs1, imm) ->
    let imm = imm_to_int32 line addr imm in
    Instructions.S.code inst rs2 rs1 imm
  | B (inst, rs1, rs2, imm) ->
    let imm = imm_to_int32 line addr imm in
    Instructions.B.code inst rs1 rs2 imm
  | U (inst, rd, imm) ->
    let imm = imm_to_int32 line addr imm in
    Instructions.U.code inst rd imm
  | J (inst, rd, imm) ->
    let imm = imm_to_int32 line addr imm in
    Instructions.J.code inst rd imm

let loop_prog mem debug labels addr prog  =
  match prog with
  | Prog_Instr (l, s, inst) ->
    Debug.add_addr_to_line debug addr l s;
    Debug.add_line_to_addr debug l addr;
    let code = translate inst addr l labels in
    Arch.Memory.set_int32 mem addr code;
    addr + 4l
  | Prog_Label _  -> addr
  | Prog_GLabel (line, label) -> Label.made_global labels label line; addr
(*No more pseudo instructions after remove_pseudo *)
  | Prog_Pseudo _ -> assert false

let loop_memory mem labels addr (prog : memory_line) =
  match prog with
  | Mem_GLabel (line, label) -> Label.made_global labels label line; addr
  | Mem_Label _  -> addr
  | Mem_Value v  -> Memory.set_int32 mem addr v; addr + 4l
  | Mem_Bytes lb ->
    List.fold_left
      (fun addr v -> Memory.set_byte mem addr (char_to_int32 v); addr + 1l)
    addr lb
  | Mem_Ascii s ->
    String.fold_left
      (fun addr v -> Memory.set_byte mem addr (char_to_int32 v); addr + 1l)
      addr s
  | Mem_Asciz s ->
    let addr =
      String.fold_left
      (fun addr v -> Memory.set_byte mem addr (char_to_int32 v); addr + 1l)
      addr s
    in
    Memory.set_byte mem addr 0l; (addr + 1l)
  | Mem_Word words ->
    List.fold_left
      (fun addr v -> Memory.set_int32 mem addr v; addr + 4l)
      addr words
  | Mem_Zero nz ->
    Memory.set_32b_zero mem addr nz;
    addr + 4l * nz

let assembly code =
  let open Arch.Segment in
  try
    let mem   = Memory.make () in
    let debug = Debug.generate_debug () in

    let prog = Parser.program Lexer.token code in

    let labels = Label.get_label_address prog in
    ignore (List.fold_left (loop_memory mem labels) static_being prog.memory);
    let prog = Transform_pseudo.remove_pseudo prog.program labels in
    ignore (List.fold_left (loop_prog mem debug labels) text_begin prog);

    (mem, labels, debug)
  with Parser.Error ->
    let pe = Parsing_error (Lexing.lexeme code) in
    raise (Assembler_error (!Lexer.line, pe))

