open Error
open Simulator
open Program

let ( * ) = Int32.mul
let ( + ) = Int32.add
let ( - ) = Int32.sub

let (&)  = Int32.logand
let (<<) = Int32.shift_left
let (>>) = Int32.shift_right_logical

let (<=) x y = Int32.compare x y <=  0
let (>=) x y = Int32.compare x y >=  0

(* Get address  ------------------------------------------------------------  *)

let global_label = Hashtbl.create 16

let label_address = Hashtbl.create 16

let addr_debug = Hashtbl.create 1024
let line_debug = Hashtbl.create 1024

let pseudo_length (pseudo : pseudo_instruction) =
  match pseudo with
  | NOP       -> 0x4l
  | LA (_, _) -> 0x8l
  | J _       -> 0x4l
  | JALP _    -> 0x4l
  | JR _      -> 0x4l
  | JALRP _   -> 0x0l
  | RET       -> 0x4l
  | CALL _    -> 0x8l
  | TAIL _    -> 0x8l
  | LGlob (_, _, _)       -> 0x8l
  | SGlob (_, _, _, _)    -> 0x8l
  | Two_Regs (_, _, _)    -> 0x4l
  | Regs_Offset (_, _, _) -> 0x4l
  | LI (_, imm) ->
    match imm with
    | Label _ -> assert false
    | Imm imm -> if -2048l <= imm && imm <= 2048l
                 then 0x4l else 0x8l

let rec get_label_address_program prog addr =
  match prog with
  | [] -> ()
  | Prog_Pseudo (_, _, instruction) :: l ->
    get_label_address_program l (addr + (pseudo_length instruction))
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
    try Hashtbl.find label_address label - addr with
    | Not_found -> raise (Assembler_error (line, Unknown_Label label))

let symbol_to_int32 line = function
  | Imm imm     -> imm
  | Label label ->
    try Hashtbl.find label_address label with
    | Not_found -> raise (Assembler_error (line, Unknown_Label label))


let hi_lo imm addr line =
  let imm = imm_to_int32 line addr imm in
  ((imm + 0x800l) >> 12, imm & 0b111111111111l)

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

let translate_two_reg (pseudo : two_reg) rd rs mem addr line =
  (match pseudo with
  | MV   -> Inst_I.write_in_memory mem addr ADDI  rd rs 0l line
  | NOT  -> Inst_I.write_in_memory mem addr XORI  rd rs (-1l) line
  | NEG  -> Inst_R.write_in_memory mem addr SUB   rd 0l rs
  | SEQZ -> Inst_I.write_in_memory mem addr SLTIU rd rs 1l line
  | SNEZ -> Inst_R.write_in_memory mem addr SLTU  rd rs 0l
  | SLTZ -> Inst_R.write_in_memory mem addr SLT   rd rs 0l
  | SGTZ -> Inst_R.write_in_memory mem addr SLT   rd 0l rs);
  4l

let translate_reg_offset (pseudo : reg_offset) rs offset mem addr =
  (match pseudo with
  | BEQZ -> Inst_B.write_in_memory mem addr BEQ rs 0l offset
  | BNEZ -> Inst_B.write_in_memory mem addr BNE rs 0l offset
  | BLEZ -> Inst_B.write_in_memory mem addr BGE 0l rs offset
  | BGEZ -> Inst_B.write_in_memory mem addr BGE rs 0l offset
  | BLTZ -> Inst_B.write_in_memory mem addr BLT rs 0l offset
  | BGTZ -> Inst_B.write_in_memory mem addr BLT 0l rs offset);
  4l

let translate_pseudo pseudo mem addr line string =
  Hashtbl.add addr_debug addr (line, string);
  match pseudo with
  | NOP       -> Inst_I.write_in_memory mem addr ADDI 0l 0l 0l line; 4l
  | LI (rd, imm) ->
    let (hi, lo) = hi_lo imm addr line in
    if hi = 0l
    then (Inst_I.write_in_memory mem addr ADDI rd 0l lo line; 4l)
    else (Hashtbl.add addr_debug (addr + 4l) (line, string);
          Inst_U.write_in_memory mem addr        LUI  rd    hi;
          Inst_I.write_in_memory mem (addr + 4l) ADDI rd rd lo line; 8l)
  | LA (rd, symbol) ->
    let (hi, lo) = hi_lo symbol addr line in
    Hashtbl.add addr_debug (addr + 4l) (line, string);
    Inst_U.write_in_memory mem addr        AUIPC rd    hi;
    Inst_I.write_in_memory mem (addr + 4l) ADDI  rd rd lo line; 8l
  | J offset  ->
    let imm = imm_to_int32 line addr offset in
    Inst_J.write_in_memory mem addr JAL 0l imm; 4l
  | JALP offset ->
    let imm = imm_to_int32 line addr offset in
    Inst_J.write_in_memory mem addr JAL 1l imm; 4l
  | JR rs     -> Inst_I.write_in_memory mem addr JALR 0l rs 0l line; 4l
  | JALRP rs  -> Inst_I.write_in_memory mem addr JALR 1l rs 0l line; 4l
  | RET       -> Inst_I.write_in_memory mem addr JALR 0l 1l 0l line; 4l
  | CALL offset ->
    let (hi, lo) = hi_lo offset addr line in
    Hashtbl.add addr_debug (addr + 4l) (line, string);
    Inst_U.write_in_memory mem addr        AUIPC 1l    hi;
    Inst_I.write_in_memory mem (addr + 4l) JALR  1l 1l lo line; 8l
  | TAIL offset ->
    let (hi, lo) = hi_lo offset addr line in
    Hashtbl.add addr_debug (addr + 4l) (line, string);
    Inst_U.write_in_memory mem addr        AUIPC 6l    hi;
    Inst_I.write_in_memory mem (addr + 4l) JALR  0l 6l lo line; 8l
  | LGlob (rd, symbol, load) ->
    let (hi, lo) = hi_lo symbol addr line in
    Hashtbl.add addr_debug (addr + 4l) (line, string);
    Inst_U.write_in_memory mem addr        AUIPC rd    hi;
    Inst_I.write_in_memory mem (addr + 4l) load  rd rd lo line; 8l
  | SGlob (rd, symbol, rt, store) ->
    let (hi, lo) = hi_lo symbol addr line in
    Hashtbl.add addr_debug (addr + 4l) (line, string);
    Inst_U.write_in_memory mem addr        AUIPC rt    hi;
    Inst_S.write_in_memory mem (addr + 4l) store rd rt lo; 8l
  | Two_Regs (inst, rd, rs) -> translate_two_reg inst rd rs mem addr line
  | Regs_Offset (inst, rs, offset) ->
    let imm = imm_to_int32 line addr offset in
    translate_reg_offset inst rs imm mem addr

let loop_prog mem addr prog =
  match prog with
  | Prog_Pseudo (l, s, inst) ->
    Hashtbl.add line_debug l addr;
    Hashtbl.add addr_debug addr (l, s);
    let length = translate_pseudo inst mem addr l s in
    addr + length
  | Prog_Instr (l, s, inst) ->
    Hashtbl.add line_debug l addr;
    Hashtbl.add addr_debug addr (l, s);
    translate inst mem addr l;
    addr + 4l
  | Prog_Label _  -> addr
  | Prog_GLabel label ->
    try
      Hashtbl.add global_label label (Hashtbl.find label_address label); addr
    with Not_found -> failwith "TODO global address"

let loop_memory mem addr (prog : memory_line) =
  match prog with
  | Mem_Value v     -> Memory.set_int32 mem addr v; addr + 4l
  | Mem_Bytes lb    ->
    List.fold_left (fun addr v ->
      Memory.set_byte mem addr (Utils.char_to_int32 v); addr + 1l)
      addr lb
  | Mem_Asciz s    ->
    let addr = String.fold_left (fun addr v ->
      Memory.set_byte mem addr (Utils.char_to_int32 v); addr + 1l)
      addr s in
    Memory.set_byte mem addr 0l; (addr + 1l)
  | Mem_Word words  ->
    List.fold_left (fun addr v ->
      Memory.set_int32 mem addr v; addr + 4l)
      addr words
  | Mem_Zero nz      ->
    Memory.set_32b_zero mem addr nz;
    addr + 4l * nz
  | Mem_Label _      -> addr
  | Mem_GLabel label ->
    try
      Hashtbl.add global_label label (Hashtbl.find label_address label); addr
    with Not_found -> failwith "TODO global address"

let translate code =
    let open Simulator.Segment in
  try
    let mem  = Memory.make () in
    let prog = Parser.program Lexer.token code in
    get_label_address prog;
    ignore (List.fold_left (loop_memory mem) static_being prog.memory);
    ignore (List.fold_left (loop_prog mem)   text_begin   prog.program);
    (mem, label_address, global_label, addr_debug, line_debug)
  with Parser.Error ->
    let pe = Parsing_error (Lexing.lexeme code) in
    raise (Assembler_error (!Lexer.line, pe))

