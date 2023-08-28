open Error
open Simulator
open Lexer
open Program

let (+) = Int32.add
let (-) = Int32.sub

let (<=) x y = Int32.compare x y <=  0
let (>=) x y = Int32.compare x y >=  0

let label_address = Hashtbl.create 16

let debug = Hashtbl.create 1024

let pseuodo_length (pseudo : pseudo_instruction) =
  match pseudo with
  | NOP -> 0x4l
  | LA (_, _) -> 0x8l
  | J _ -> 0x4l
  | JALI _ -> 0x4l
  | JR _ -> 0x4l
  | JALR _ -> 0x0l
  | RET -> 0x8l
  | CALL _ -> 0x8l
  | TAIL _ -> 0x8l
  | LGlob (_, _, _) -> 0x8l
  | SGlob (_, _, _) -> 0x8l
  | Two_Regs (_, _, _) -> 0x4l
  | Regs_Offset (_, _, _) -> 0x4l
  | LI (_, imm) ->
    match imm with
    | Label _ -> 0x8l
    | Imm imm -> if -2048l <= imm && imm <= 2048l
                 then 0x4l else 0x8l



let rec get_label_address prog addr =
  match prog with
  | Nil -> ()
  | Seq (Instr (_, _, Pseudo instruction), l) ->
    get_label_address l (addr + (pseuodo_length instruction))
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
  | Seq (Instr (_l, _s, Pseudo _), _next) -> failwith "TODO\n"
  | Seq (Label _, l) -> write_in_memory l mem addr

let translate code =
  let mem = Memory.make () in
  let prog = prog 0 code in
  get_label_address prog Segment.text_begin;
  let addr = write_in_memory prog mem Segment.text_begin in
  (mem, addr, label_address, debug)

