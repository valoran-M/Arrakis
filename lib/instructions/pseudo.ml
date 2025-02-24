(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Insts

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

