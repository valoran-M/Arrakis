(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

let regs = Hashtbl.create 63
let () =
  List.iteri (fun i x -> Hashtbl.add regs x (Int32.of_int i))
  [
    "zero";
    "ra"  ; "sp"  ; "gp"  ; "tp"  ;
    "t0"  ; "t1"  ; "t2"  ;
    "s0"  ; "s1"  ;
    "a0"  ; "a1"  ; "a2"  ; "a3"  ; "a4"  ; "a5"  ; "a6"  ; "a7"  ;
    "s2"  ; "s3"  ; "s4"  ; "s5"  ; "s6"  ; "s7"  ; "s8"  ; "s9"  ;
    "s10" ; "s11" ;
    "t3"  ; "t4"  ; "t5"  ; "t6"  ;
  ];
  Hashtbl.add regs "fp" 8l;
  for i = 0 to 31 do
    Hashtbl.add regs ("x" ^ (Int.to_string i)) (Int32.of_int i)
  done

let of_string =
  Hashtbl.find regs
