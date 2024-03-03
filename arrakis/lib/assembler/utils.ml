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

let imm_to_int32 labels line addr = function
  | Imm   imm   -> imm
  | Label label -> Label.get_address labels label line - addr

let hi_lo imm addr line label_address =
  let imm = imm_to_int32 label_address line addr imm in
  ((imm + 0x800l) >> 12, imm & 0b111111111111l)

