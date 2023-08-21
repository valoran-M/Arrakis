open Simulator
open Lexer
open Program

let (+) = Int32.add

let label_address = Hashtbl.create 16

let rec get_label_address prog addr =
  match prog with
  | Nil -> ()
  | Seq (Instr (_, _), l)               -> get_label_address l (addr + 0x4l)
  | Seq (Label label, l)  ->
    Hashtbl.add label_address label addr;
    get_label_address l (addr + 0x4l)

let imm_to_int32 = function
  | Imm imm     -> imm
  | Label label -> Hashtbl.find label_address label

let rec write_in_memory prog mem addr =
  match prog with
  | Nil -> ()
  | Seq (Instr (_, R (inst, rd, rs1, rs2)), l) ->
    Inst_R.write_in_memory mem addr inst rd rs1 rs2;
    write_in_memory l mem (addr + 4l)
  | Seq (Instr (_, I (inst, rd, rs1, imm)), l) ->
    Inst_I.write_in_memory mem addr inst rd rs1 (imm_to_int32 imm);
    write_in_memory l mem (addr + 4l)
  | Seq (Instr (_, S (inst, rs2, rs1, imm)), l) ->
    Inst_S.write_in_memory mem addr inst rs2 rs1 (imm_to_int32 imm);
    write_in_memory l mem (addr + 4l)
  | Seq (Instr (_, B (inst, rs1, rs2, imm)), l) ->
    Inst_B.write_in_mem mem addr inst rs1 rs2 (imm_to_int32 imm);
    write_in_memory l mem (addr + 4l)
  | Seq (Instr (_, U (inst, rd, imm)), l) ->
    Inst_U.write_in_memory mem addr inst rd (imm_to_int32 imm);
    write_in_memory l mem (addr + 4l)
  | Seq (Instr (_, J (inst, rd,  imm)), l) ->
    Inst_J.write_in_mem mem addr inst rd (imm_to_int32 imm);
    write_in_memory l mem (addr + 4l)
  | Seq (Label _, l) -> write_in_memory l mem addr

let translate code =
  let mem = Memory.make () in
  let prog = prog 0 (Lexing.from_string code) in
  get_label_address prog Segment.text_begin;
  write_in_memory prog mem Segment.text_begin;
  mem

