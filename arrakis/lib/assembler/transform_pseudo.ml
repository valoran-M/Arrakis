(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Error
open Program

(*
  The assembly code contains pseudo-instructions, so we're going to try and
  transform them into pure RISC-V instructions to make the final assembly
  easier.
*)

let ( * ) = Int32.mul
let ( + ) = Int32.add
let ( - ) = Int32.sub

let (&)  = Int32.logand
let (<<) = Int32.shift_left
let (>>) = Int32.shift_right_logical


let imm_to_int32 label_address line addr = function
  | Imm imm     -> imm
  | Label label ->
    try  Hashtbl.find label_address label - addr
    with Not_found -> raise (Assembler_error (line, Unknown_Label label))

let hi_lo imm addr line label_address =
  let imm = imm_to_int32 label_address line addr imm in
  Printf.printf "%s %s\n" (Int32.to_string imm) (Int32.to_string addr);
  ((imm + 0x800l) >> 12, imm & 0b111111111111l)

(*
  Function that translates pseudo instructions

  To obtain a terminal recurrence, one inverts the generated list.

  /!\ If the pseudo instruction generates several instructions, the instructions
      must be reversed.
*)
let translate_pseudo pseudo line code addr label_address =

  let translate_j offset =
    let imm = imm_to_int32 label_address line addr offset in
    [Prog_Instr (line, code, J(JAL, 0l, Imm imm))]
  in

  let translate_jalp offset =
    let imm = imm_to_int32 label_address line addr offset in
    [Prog_Instr (line, code, J(JAL, 1l, Imm imm))]
  in

  let translate_li rd imm =
    let (hi, lo) = hi_lo imm addr line label_address in
    if hi = 0l
    then [Prog_Instr (line, code, I(ADDI, rd, 0l, Imm lo))]
    else [ Prog_Instr (line, code, I(ADDI, rd, 0l, Imm lo));
           Prog_Instr (line, code, U(LUI,  rd,     Imm hi))]
  in

  let translate_la rd label =
    let (hi, lo) = hi_lo label addr line label_address in
    [ Prog_Instr (line, code, I(ADDI,  rd, rd, Imm lo));
      Prog_Instr (line, code, U(AUIPC, rd,     Imm hi))]
  in

  let translate_call offset =
    let (hi, lo) = hi_lo offset addr line label_address in
    [ Prog_Instr (line, code, I(JALR,  1l, 1l, Imm lo));
      Prog_Instr (line, code, U(AUIPC, 1l,     Imm hi))]
  in

  let translate_tail offset =
    let (hi, lo) = hi_lo offset addr line label_address in
    [ Prog_Instr (line, code, I(JALR,  0l, 6l, Imm lo));
      Prog_Instr (line, code, U(AUIPC, 6l,     Imm hi))]
  in

  let translate_lglob rd symbol load =
    let (hi, lo) = hi_lo symbol addr line label_address in
    [ Prog_Instr (line, code, I(load,  rd, rd, Imm lo));
      Prog_Instr (line, code, U(AUIPC, rd,     Imm hi))]
  in

  let translate_sglob rd symbol rt store =
    let (hi, lo) = hi_lo symbol addr line label_address in
    [ Prog_Instr (line, code, S(store, rd, rt, Imm lo));
      Prog_Instr (line, code, U(AUIPC, rd,     Imm hi))]
  in

  let translate_two_reg inst rd rs =
    match inst with
    | MV   -> [Prog_Instr(line, code, I(ADDI, rd, rs, Imm 0l   ))]
    | NOT  -> [Prog_Instr(line, code, I(XORI, rd, rs, Imm (-1l)))]
    | NEG  -> [Prog_Instr(line, code, R(SUB,  rd, 0l, rs       ))]
    | SEQZ -> [Prog_Instr(line, code, I(SLTIU,rd, rs, Imm 1l   ))]
    | SNEZ -> [Prog_Instr(line, code, R(SLTU, rd, rs, 0l       ))]
    | SLTZ -> [Prog_Instr(line, code, R(SLT,  rd, rs, 0l       ))]
    | SGTZ -> [Prog_Instr(line, code, R(SLT,  rd, 0l, rs       ))]
  in

  let translate_reg_offset inst rs offset =
    let imm = Imm (imm_to_int32 label_address line addr offset) in
    match inst with
    | BEQZ -> [Prog_Instr(line, code, B(BEQ, rs, 0l, imm))]
    | BNEZ -> [Prog_Instr(line, code, B(BNE, rs, 0l, imm))]
    | BLEZ -> [Prog_Instr(line, code, B(BGE, 0l, rs, imm))]
    | BGEZ -> [Prog_Instr(line, code, B(BGE, rs, 0l, imm))]
    | BLTZ -> [Prog_Instr(line, code, B(BLT, rs, 0l, imm))]
    | BGTZ -> [Prog_Instr(line, code, B(BLT, 0l, rs, imm))]
  in

  let translate_reg_reg_offset inst rs rt offset =
    let imm = Imm (imm_to_int32 label_address line addr offset) in
    match inst with
    | BGT  -> [Prog_Instr(line, code, B(BLT,  rt, rs, imm))]
    | BLE  -> [Prog_Instr(line, code, B(BGE,  rt, rs, imm))]
    | BGTU -> [Prog_Instr(line, code, B(BLTU, rt, rs, imm))]
    | BLEU -> [Prog_Instr(line, code, B(BGEU, rt, rs, imm))]
  in

  match pseudo with
  | NOP             -> [Prog_Instr (line, code, I(ADDI, 0l, 0l, Imm 0l))]
  | LI (rd, imm)    -> translate_li rd imm
  | LA (rd, label)  -> translate_la rd label
  | J offset        -> translate_j offset
  | JALP offset     -> translate_jalp offset
  | JR rs           -> [Prog_Instr (line, code, I(JALR, 0l, rs, Imm 0l))]
  | JALRP rs        -> [Prog_Instr (line, code, I(JALR, 1l, rs, Imm 0l))]
  | RET             -> [Prog_Instr (line, code, I(JALR, 1l, 1l, Imm 0l))]
  | CALL offset                         -> translate_call offset
  | TAIL offset                         -> translate_tail offset
  | LGlob (rd, symbol, load)            -> translate_lglob rd symbol load
  | SGlob (rd,symbol,rt,store)          -> translate_sglob rd symbol rt store
  | Two_Regs (inst, rd, rs)             -> translate_two_reg inst rd rs
  | Regs_Offset (inst, rs, offset)      -> translate_reg_offset inst rs offset
  | Regs_Regs_Offset(inst,rs,rt,offset) -> translate_reg_reg_offset inst rs rt offset

let remove_pseudo prog label_address =
  let rec iterator prog addr acc =
    match prog with
    | [] -> acc
    | Prog_Pseudo (line, code, inst) :: prog ->
      let instr = translate_pseudo inst line code addr label_address in
      let size = List.length instr in
      iterator prog (addr + 4l * Int32.of_int size) (instr @ acc)
    | (Prog_Instr _ as inst) :: prog -> iterator prog (addr + 4l) (inst :: acc)
    | (_ as inst)            :: prog -> iterator prog addr        (inst :: acc)

  in
  let open Simulator.Segment in
  iterator prog text_begin [] |> List.rev

