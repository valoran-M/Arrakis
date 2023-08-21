open Simulator
open Lexer
open Program

let (+) = Int32.add

let label_address = Hashtbl.create 16

let rec get_label_address prog addr =
  match prog with
  | Nil -> ()
  | Seq (Instr (_, I (LI, _, _, _)), l) -> get_label_address l (addr + 0x8l)
  | Seq (Instr (_, _), l)               -> get_label_address l (addr + 0x4l)
  | Seq (Label label, l)  ->
    Hashtbl.add label_address label addr;
    get_label_address l (addr + 0x4l)

let rec write_in_memory prog mem addr =
  match prog with
  | Nil -> ()
  | Seq (Instr (_, R (inst, rd,  rs1, rs2)), l) ->
    write_in_memory l mem
      (addr + Inst_R.write_in_memory mem addr inst rd rs1 rs2)
  | Seq (Instr (_, I (_inst, _rd,  _rs1, _imm)), _l) -> failwith "TODO"
  | Seq (Instr (_, S (_inst, _rs2, _rs1, _imm)), _l) -> failwith "TODO"
  | Seq (Instr (_, B (_inst, _rs1, _rs2, _imm)), _l) -> failwith "TODO"
  | Seq (Instr (_, U (_inst, _rd,  _imm)),      _l) -> failwith "TODO"
  | Seq (Instr (_, J (_inst, _rd,  _imm)),      _l) -> failwith "TODO"
  | Seq (Label _, l) -> write_in_memory l mem addr

let translate code =
  let mem = Memory.make () in
  let prog = prog 0 (Lexing.from_string code) in
  get_label_address prog Segment.text_begin;
  write_in_memory prog mem Segment.text_begin;
  mem

