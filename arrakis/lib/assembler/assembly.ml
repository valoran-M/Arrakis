(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
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

open Gutils.Integer
open Instructions.Insts
open Utils
open Error
open Program
open Arch

let translate (instruction : basics_inst) addr line labels =
  let imm_to_int32 = imm_to_int32 labels in
  match instruction with
  | R (inst, rd, rs1, rs2) ->
      Instructions.R.code inst rd rs1 rs2
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
  | Text_Label _  -> addr
  | Text_GLabel (line, label) -> Label.made_global labels label line; addr
  | Text_Instr (l, s, inst) ->
    Debug.add_addr_to_line debug addr l s;
    Debug.add_line_to_addr debug l addr;
    let code = translate inst addr l labels in
    Arch.Memory.set_int32 mem addr code;
    addr + 4l
(* No more pseudo instructions after remove_pseudo *)
  | Text_Pseudo _ -> assert false

let loop_memory mem labels addr (prog : data_line) =
  let open String in
  match prog with
  | Data_GLabel (line, label) -> Label.made_global labels label line; addr
  | Data_Label _ -> addr
  | Data_Zero nz -> Memory.set_32b_zero mem addr nz; addr + 4l * nz
  | Data_Ascii ls ->
      List.fold_left (fun a s -> Memory.set_str  mem a s (length s)) addr ls
  | Data_Asciz ls ->
      List.fold_left (fun a s -> Memory.set_strz mem a s (length s)) addr ls
  | Data_Bytes lb ->
    List.fold_left
      (fun addr v -> Memory.set_byte mem addr (char_to_int32 v); addr + 1l)
      addr lb
  | Data_Word words ->
    List.fold_left
      (fun addr v -> Memory.set_int32 mem addr v; addr + 4l)
      addr words

let assembly code =
  let open Arch.Segment in
  try
    let mem = Memory.make () in
    let dbg = Debug.generate_debug () in

    let prog = Parser.program Lexer.token code in

    let labels = Label.get_label_address prog in
    ignore (List.fold_left (loop_memory mem labels) static_begin prog.data);
    let prog = Pseudo.remove_pseudo prog.text labels in
    ignore (List.fold_left (loop_prog mem dbg labels) text_begin prog);

    (mem, labels, dbg)
  with Parser.Error ->
    let pe = Parsing_error (Lexing.lexeme code) in
    raise (Assembler_error (!Lexer.line, pe))

