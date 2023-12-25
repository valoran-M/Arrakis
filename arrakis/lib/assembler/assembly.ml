(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Utils
open Error
open Program

(* Get address  ------------------------------------------------------------  *)

let global_label = Hashtbl.create 16

let label_address = Hashtbl.create 16

let rec get_label_address_program prog addr =
  match prog with
  | [] -> ()
  | Prog_Pseudo (_, _, instruction) :: l ->
    get_label_address_program l (addr + (Inst_Pseudo.pseudo_length instruction))
  | Prog_Instr (_, _, _) :: l -> get_label_address_program l (addr + 0x4l)
  | Prog_GLabel _ :: l -> get_label_address_program l addr
  | Prog_Label label  :: l ->
    Hashtbl.add label_address label addr;
    get_label_address_program l addr

let rec get_label_address_memory (memory : memory_line list) addr =
  match memory with
  | [] -> ()
  | Mem_Value _     :: l -> get_label_address_memory l (addr + 0x4l)
  | Mem_Bytes bs    :: l ->
    get_label_address_memory l (addr + Int32.of_int (List.length bs))
  | Mem_Asciz s    :: l ->
    get_label_address_memory l (addr + Int32.of_int (String.length s) + 1l)
  | Mem_Word lw     :: l ->
    let offset = 0x4l * Int32.of_int (List.length lw) in
    get_label_address_memory l (addr + offset)
  | Mem_Zero nz     :: l -> get_label_address_memory l (addr + 4l * nz);
  | Mem_GLabel _    :: l -> get_label_address_memory l addr
  | Mem_Label label :: l ->
    Hashtbl.add label_address label addr;
    get_label_address_memory l addr

let get_label_address (prog : program) =
  let open Simulator.Segment in
  get_label_address_memory  prog.memory  static_being;
  get_label_address_program prog.program text_begin

(* Translation -------------------------------------------------------------  *)

let imm_to_int32 line addr = function
  | Imm imm     -> imm
  | Label label ->
    try  Hashtbl.find label_address label - addr
    with Not_found -> raise (Assembler_error (line, Unknown_Label label))

let symbol_to_int32 line = function
  | Imm imm     -> imm
  | Label label ->
    try  Hashtbl.find label_address label
    with Not_found -> raise (Assembler_error (line, Unknown_Label label))

let translate (instruction : instruction) mem addr line =
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

let loop_prog mem debug addr prog =
  match prog with
  | Prog_Instr (l, s, inst) ->
    Debug.add_addr_to_line debug addr l s;
    Debug.add_line_to_addr debug l addr;
    translate inst mem addr l;
    addr + 4l
  (* No more pseudo instructions after remove_pseudo *)
  | Prog_Pseudo _ -> assert false
  | Prog_Label _  -> addr
  | Prog_GLabel (line, label) ->
    try
      Hashtbl.add global_label label (Hashtbl.find label_address label); addr
    with Not_found ->
      raise (Assembler_error (line, (Error.Unknown_Label label)))

let loop_memory mem addr (prog : memory_line) =
  match prog with
  | Mem_Value v     -> Simulator.Memory.set_int32 mem addr v; addr + 4l
  | Mem_Bytes lb    ->
    List.fold_left (fun addr v ->
      Simulator.Memory.set_byte mem addr (Simulator.Utils.char_to_int32 v);
      addr + 1l)
    addr lb
  | Mem_Asciz s    ->
    let addr =
      String.fold_left (fun addr v ->
        Simulator.Memory.set_byte mem addr (Simulator.Utils.char_to_int32 v);
        addr + 1l)
      addr s
    in
    Simulator.Memory.set_byte mem addr 0l; (addr + 1l)
  | Mem_Word words  ->
    List.fold_left (fun addr v ->
      Simulator.Memory.set_int32 mem addr v; addr + 4l)
      addr words
  | Mem_Zero nz      ->
    Simulator.Memory.set_32b_zero mem addr nz;
    addr + 4l * nz
  | Mem_Label _      -> addr
  | Mem_GLabel (line, label) ->
    try
      Hashtbl.add global_label label (Hashtbl.find label_address label); addr
    with Not_found ->
      raise (Assembler_error (line, (Error.Unknown_Label label)))

let assembly code =
    let open Simulator.Segment in
  try
    let mem  = Simulator.Memory.make () in
    let debug = Debug.generate_debug () in
    let prog = Parser.program Lexer.token code in

    get_label_address prog;
    ignore (List.fold_left (loop_memory mem) static_being prog.memory);
    let prog = Transform_pseudo.remove_pseudo prog.program label_address in
    ignore (List.fold_left (loop_prog mem debug) text_begin prog);

    (mem, label_address, global_label, debug)
  with Parser.Error ->
    let pe = Parsing_error (Lexing.lexeme code) in
    raise (Assembler_error (!Lexer.line, pe))

