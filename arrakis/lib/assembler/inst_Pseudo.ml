(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Program

let two_regs_str = Hashtbl.create 7

let () =
  List.iter (fun (v, k) -> Hashtbl.add two_regs_str k v)
    [
  (*  inst  str   *)
      MV,   "mv"  ;
      NOT,  "not" ;
      NEG,  "neg" ;
      SEQZ, "seq" ;
      SNEZ, "snez";
      SLTZ, "sltz";
      SGTZ, "sgtz";
    ]

let regs_offset_str = Hashtbl.create 6

let () =
  List.iter (fun (v, k) -> Hashtbl.add regs_offset_str k v)
    [
  (*  inst  str   *)
      BEQZ, "beqz";
      BNEZ, "bnez";
      BLEZ, "blez";
      BGEZ, "bgez";
      BLTZ, "bltz";
      BGTZ, "bgtz";
    ]

let regs_regs_offset_str = Hashtbl.create 4

let () =
  List.iter (fun (v, k) -> Hashtbl.add regs_regs_offset_str k v)
    [
  (*  inst  str   *)
      BGT,  "bgt";
      BLE,  "ble";
      BGTU, "bgtu";
      BLEU, "bleu";
    ]

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
  | LGlob       (_, _, _) -> 0x8l
  | SGlob    (_, _, _, _) -> 0x8l
  | Two_Regs    (_, _, _) -> 0x4l
  | Regs_Offset (_, _, _) -> 0x4l
  | Regs_Regs_Offset (_, _, _, _) -> 0x4l
  | LI (_, imm) ->
    match imm with
    | Label _ -> assert false
    | Imm imm -> if -2048l <= imm && imm <= 2048l
                 then 0x4l else 0x8l

