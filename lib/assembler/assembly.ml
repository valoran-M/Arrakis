(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

(*
  Translation of assembly code to byte code.

  parser ->
  get_label_address ->
  remove pseudo instruction ->
  write instruction in memory
*)

open Instructions.Insts
open Utils
open Error
open Program
open Arch

let translate (instruction : basics_inst) addr line labels =
  match instruction with
  | R (inst, rd, rs1, rs2) -> Instructions.R.code inst rd rs1 rs2
  | I (inst, rd, rs1, imm) ->
    let imm = expr_to_int32_rel imm labels line addr in
    Instructions.I.code inst rd rs1 imm
  | S (inst, rs2, rs1, imm) ->
    let imm = expr_to_int32_rel imm labels line addr in
    Instructions.S.code inst rs2 rs1 imm
  | B (inst, rs1, rs2, imm) ->
    let imm = expr_to_int32_rel imm labels line addr in
    Instructions.B.code inst rs1 rs2 imm
  | U (inst, rd, imm) ->
    let imm = expr_to_int32_rel imm labels line addr in
    Instructions.U.code inst rd imm
  | J (inst, rd, imm) ->
    let imm = expr_to_int32_rel imm labels line addr in
    Instructions.J.code inst rd imm

let exec_directive directive labels addr =
  match directive with
  | Glob_GLabel (line, label) -> Label.made_global labels label line; addr
  | Glob_Size (line, label, expr) ->
    let size = Int32.to_int (Utils.expr_to_int32 expr labels line addr) in
    Label.set_size labels label size;
    addr

let loop_text mem debug labels addr prog  =
  match prog with
  | Text_Label _ -> addr
  | Text_Direc d -> exec_directive d labels addr
  | Text_Instr  (l, s, inst)  ->
    Debug.add_addr_to_line debug addr l s;
    Debug.add_line_to_addr debug l addr;
    let code = translate inst addr l labels in
    Arch.Memory.set_int32 mem addr code;
    addr + 4l
(* No more pseudo instructions after remove_pseudo *)
  | Text_Pseudo _ -> assert false

let loop_data mem _ labels addr (prog : data_line) =
  let open String in
  let open Memory in
  let set_str  a s = set_str  mem a s (length s) in
  let set_strz a s = set_strz mem a s (length s) in
  let set_byte a e = set_byte  mem a (expr_to_char  e labels (-1) a); a + 1l in
  let set_word a e = set_int32 mem a (expr_to_int32 e labels (-1) a); a + 4l in
  match prog with
  | Data_Direc d  -> exec_directive d labels addr
  | Data_Label _  -> addr
  | Data_Zero nz  -> Memory.set_32b_zero mem addr nz; addr + 4l * nz
  | Data_Ascii ls -> List.fold_left set_str  addr ls
  | Data_Asciz ls -> List.fold_left set_strz addr ls
  | Data_Bytes lb -> List.fold_left set_byte addr lb
  | Data_Word  lw -> List.fold_left set_word addr lw 

let assembly code =
  let open Arch.Segment in
  try
    let mem = Memory.make () in
    let dbg = Debug.generate_debug () in

    let prog = Parser.program Lexer.token code in

    let prog = { prog with text = Pseudo.remove_pseudo prog.text } in
    let lbls = Label.get_label_address prog in
    ignore (List.fold_left (loop_text mem dbg lbls) text_begin prog.text);
    ignore (List.fold_left (loop_data mem dbg lbls) data_begin prog.data);

    mem, lbls, dbg
  with Parser.Error ->
    let pe = Parsing_error (Lexing.lexeme code) in
    raise (Assembler_error (!Lexer.line, pe))

