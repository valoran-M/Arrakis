(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions.Insts

let (&)  = Int32.logand
let (<<) = Int32.shift_left
let (>>) = Int32.shift_right_logical

let get_interval imm i j =
  let open Int32 in
  let mask = lognot (-1l << (i - j + 1)) in
  (imm >> j) & mask

let ( * ) = Int32.mul
let ( + ) = Int32.add
let ( - ) = Int32.sub

let (<=) x y = Int32.compare x y <=  0
let (>=) x y = Int32.compare x y >=  0

let hi_lo imm = (imm + 0x800l) >> 12, imm & 0b111111111111l

let rec expr_to_int32 expr labels line addr =
  match expr with
  | Imm i -> i
  | Hig e -> fst (hi_lo (expr_to_int32 e labels line addr))
  | Low e -> snd (hi_lo (expr_to_int32 e labels line addr))
  | Lbl l -> Label.get_address labels l line - addr
  | Add (e1, e2)  -> expr_to_int32 e1 labels line addr +
                     expr_to_int32 e2 labels line addr
  | Sub (e1, e2)  -> expr_to_int32 e1 labels line addr -
                     expr_to_int32 e2 labels line addr

