open Error
open Simulator
open Lexer
open Program

let (+) = Int32.add
let (-) = Int32.sub

let label_address = Hashtbl.create 16

let debug = Hashtbl.create 1024

let rec get_label_address prog addr =
  match prog with
  | Nil -> ()
  | Seq (Instr (_, _, _), l) -> get_label_address l (addr + 0x4l)
  | Seq (Label label, l)  ->
    Hashtbl.add label_address label addr;
    get_label_address l (addr + 0x4l)

let imm_to_int32 line addr = function
  | Imm imm     -> imm
  | Label label ->
    try Hashtbl.find label_address label - addr with
    | Not_found -> raise (Translate_error (line, Label_not_exists label))

let rec write_in_memory prog mem addr =
  match prog with
  | Nil -> Memory.set_int32 mem addr 0l; addr
  | Seq (Instr (l, s, R (inst, rd, rs1, rs2)), next) ->
    Hashtbl.add debug addr (l, s);
    Inst_R.write_in_memory mem addr inst rd rs1 rs2;
    write_in_memory next mem (addr + 4l)
  | Seq (Instr (l, s, I (inst, rd, rs1, imm)), next) ->
    Hashtbl.add debug addr (l, s);
    Inst_I.write_in_memory mem addr inst rd rs1 (imm_to_int32 l addr imm);
    write_in_memory next mem (addr + 4l)
  | Seq (Instr (l, s, S (inst, rs2, rs1, imm)), next) ->
    Hashtbl.add debug addr (l, s);
    Inst_S.write_in_memory mem addr inst rs2 rs1 (imm_to_int32 l addr imm);
    write_in_memory next mem (addr + 4l)
  | Seq (Instr (l, s, B (inst, rs1, rs2, imm)), next) ->
    Hashtbl.add debug addr (l, s);
    Inst_B.write_in_mem mem addr inst rs1 rs2 (imm_to_int32 l addr imm);
    write_in_memory next mem (addr + 4l)
  | Seq (Instr (l, s, U (inst, rd, imm)), next) ->
    Hashtbl.add debug addr (l, s);
    Inst_U.write_in_memory mem addr inst rd (imm_to_int32 l addr imm);
    write_in_memory next mem (addr + 4l)
  | Seq (Instr (l, s, J (inst, rd,  imm)), next) ->
    Hashtbl.add debug addr (l, s);
    Inst_J.write_in_mem mem addr inst rd (imm_to_int32 l addr imm);
    write_in_memory next mem (addr + 4l)
  | Seq (Label _, l) -> write_in_memory l mem addr

let translate code =
  let mem = Memory.make () in
  let prog = prog 0 code in
  get_label_address prog Segment.text_begin;
  let addr = write_in_memory prog mem Segment.text_begin in
  (mem, addr, label_address, debug)

