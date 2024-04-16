(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions.Insts
open Utils
open Program

(*
  The assembly code contains pseudo-instructions, so we're going to
  transform them into pure RISC-V instructions to make the final assembly
  easier.
*)

(*
  To obtain a terminal recursion, one inverts the generated list.

  /!\ If the pseudo instruction generates several instructions, the instructions
      must be reversed.
*)
let translate_pseudo pseudo line code =
  let hi_lo_imm imm = Hig imm, Sub (Low imm, Imm (-4l)) in

  let li rd imm =
    match imm with
    | Low _ -> [ Text_Instr (line, code, I (ADDI, rd, 0l, imm)) ]
    | Hig _ -> [ Text_Instr (line, code, U (LUI,  rd,     imm)) ]
    | Imm z ->
      let hi, lo = hi_lo z in
      if hi = 0l
      then [ Text_Instr (line, code, I (ADDI, rd, 0l, Imm lo)) ]
      else
        let hi, lo = hi_lo_imm imm in
        [ Text_Instr (line, code, I (ADDI, rd, 0l, lo)) ;
          Text_Instr (line, code, U (LUI,  rd,     hi)) ]
    | _ -> 
      let hi, lo = hi_lo_imm imm in
      [ Text_Instr (line, code, I (ADDI, rd, 0l, lo)) ;
        Text_Instr (line, code, U (LUI,  rd,     hi)) ]
  in

  let la rd imm =
    let hi, lo = hi_lo_imm imm in
    [ Text_Instr (line, code, I (ADDI,  rd, rd, lo)) ;
      Text_Instr (line, code, U (AUIPC, rd,     hi)) ]
  in

  let call imm =
    let hi, lo = hi_lo_imm imm in
    [ Text_Instr (line, code, I (JALR,  1l, 1l, lo)) ;
      Text_Instr (line, code, U (AUIPC, 1l,     hi)) ]
  in

  let tail imm =
    let hi, lo = hi_lo_imm imm in
    [ Text_Instr (line, code, I (JALR,  0l, 6l, lo)) ;
      Text_Instr (line, code, U (AUIPC, 6l,     hi)) ]
  in

  let lglob rd imm load =
    let hi, lo = hi_lo_imm imm in
    [ Text_Instr (line, code, I (load,  rd, rd, lo)) ;
      Text_Instr (line, code, U (AUIPC, rd,     hi)) ]
  in

  let sglob rd imm rt store =
    let hi, lo = hi_lo_imm imm in
    [ Text_Instr (line, code, S (store, rd, rt, lo)) ;
      Text_Instr (line, code, U (AUIPC, rd,     hi)) ]
  in

  let two_reg inst rd rs =
    match inst with
    | MV   -> [ Text_Instr (line, code, I (ADDI,  rd, rs, Imm 0l   )) ]
    | NOT  -> [ Text_Instr (line, code, I (XORI,  rd, rs, Imm (-1l))) ]
    | NEG  -> [ Text_Instr (line, code, R (SUB,   rd, 0l, rs       )) ]
    | SEQZ -> [ Text_Instr (line, code, I (SLTIU, rd, rs, Imm 1l   )) ]
    | SNEZ -> [ Text_Instr (line, code, R (SLTU,  rd, rs, 0l       )) ]
    | SLTZ -> [ Text_Instr (line, code, R (SLT,   rd, rs, 0l       )) ]
    | SGTZ -> [ Text_Instr (line, code, R (SLT,   rd, 0l, rs       )) ]
  in

  let reg_offset inst rs imm=
    match inst with
    | BEQZ -> [ Text_Instr (line, code, B (BEQ, rs, 0l, imm)) ]
    | BNEZ -> [ Text_Instr (line, code, B (BNE, rs, 0l, imm)) ]
    | BLEZ -> [ Text_Instr (line, code, B (BGE, 0l, rs, imm)) ]
    | BGEZ -> [ Text_Instr (line, code, B (BGE, rs, 0l, imm)) ]
    | BLTZ -> [ Text_Instr (line, code, B (BLT, rs, 0l, imm)) ]
    | BGTZ -> [ Text_Instr (line, code, B (BLT, 0l, rs, imm)) ]
  in

  let reg_reg_offset inst rs rt imm =
    match inst with
    | BGT  -> [ Text_Instr (line, code, B (BLT,  rt, rs, imm)) ]
    | BLE  -> [ Text_Instr (line, code, B (BGE,  rt, rs, imm)) ]
    | BGTU -> [ Text_Instr (line, code, B (BLTU, rt, rs, imm)) ]
    | BLEU -> [ Text_Instr (line, code, B (BGEU, rt, rs, imm)) ]
  in

  match pseudo with
  | LI (rd, imm)    -> li rd imm
  | LA (rd, label)  -> la rd label
  | CALL offset     -> call offset
  | TAIL offset     -> tail offset
  | J offset        -> [ Text_Instr (line, code, J (JAL, 0l, offset)) ]
  | JALP offset     -> [ Text_Instr (line, code, J (JAL, 1l, offset)) ]
  | NOP             -> [ Text_Instr (line, code, I (ADDI, 0l, 0l, Imm 0l)) ]
  | JR rs           -> [ Text_Instr (line, code, I (JALR, 0l, rs, Imm 0l)) ]
  | JALRP rs        -> [ Text_Instr (line, code, I (JALR, 1l, rs, Imm 0l)) ]
  | RET             -> [ Text_Instr (line, code, I (JALR, 1l, 1l, Imm 0l)) ]
  | LGlob (rd, symbol, load)      -> lglob rd symbol load
  | SGlob (rd, symbol, rt, store) -> sglob rd symbol rt store
  | Two_Regs (inst, rd, rs)                 -> two_reg inst rd rs
  | Regs_Offset (inst, rs, offset)          -> reg_offset inst rs offset
  | Regs_Regs_Offset (inst, rs, rt, offset) -> reg_reg_offset inst rs rt offset

let remove_pseudo prog =
  let rec iterator prog acc =
    match prog with
    | [] -> acc
    | Text_Instr _ as inst :: prog -> iterator prog (inst :: acc)
    | Text_Pseudo (line, code, inst) :: prog ->
      let instr = translate_pseudo inst line code in
      iterator prog  (instr @ acc)
    | (_ as inst) :: prog -> iterator prog (inst :: acc)
  in
  iterator prog [] |> List.rev

