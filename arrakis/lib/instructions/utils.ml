(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(* Instructions format ------------------------------------------------------ *)

(* Instruction format :
   31          25 24      20 19      15 14  12 11         7 6            0
  +-----------------------------------------------------------------------+
  | funct7       | rs2      | rs1      |funct3| rd         | opcode       | R
  | imm[11:0]               | rs1      |funct3| rd         | opcode       | I
  | imm[11:5]    | rs2      | rs1      |funct3| imm[4:0]   | opcode       | S
  | imm[12|10:5] | rs2      | rs1      |funct3| imm[4:1|11]| opcode       | B
  | imm[31:12]                                | rd         | opcode       | U
  | imm[20|10:1|11|19:12]                     | rd         | opcode       | J
  +-----------------------------------------------------------------------+
*)

let rd_mask    = Int32.of_int 0b00000000000000000000111110000000
let func3_mask = Int32.of_int 0b00000000000000000111000000000000
let rs1_mask   = Int32.of_int 0b00000000000011111000000000000000
let rs2_mask   = Int32.of_int 0b00000001111100000000000000000000
let func7_mask = Int32.of_int 0b11111110000000000000000000000000
let imm12_mask = Int32.of_int 0b11111111111100000000000000000000
let imm20_mask = Int32.of_int 0b11111111111111111111000000000000

(* Int 32 operator ---------------------------------------------------------- *)

(* Basic arithmetic *)

let (-)   = Int32.sub
let (+)   = Int32.add
let (^)   = Int32.logxor
let (||)  = Int32.logor
let (& )  = Int32.logand
let ( * ) = Int32.mul
let (/)   = Int32.div
let (/.)  = Int32.unsigned_div
let (%)   = Int32.rem
let (%.)  = Int32.unsigned_rem

(* Multiplication *)

let high x = Int64.to_int32 (Int64.shift_right_logical x 32)

let u32_to_i64 x =
  let open Int64 in
  shift_right_logical (shift_left (of_int32 x) 32) 32

let mulh x y =
  let open Int64 in
  let x = of_int32 x in
  let y = of_int32 y in
  high (mul x y)

let mulhu x y =
  let open Int64 in
  let x = u32_to_i64 x in
  let y = u32_to_i64 y in
  high (mul x y)

let mulhsu x y =
  let open Int64 in
  let x = of_int32 x in
  let y = u32_to_i64 y in
  high (mul x y)

(* Logical base *)

let (<<)  x y = Int32.shift_left          x (Int32.to_int y)
let (>>)  x y = Int32.shift_right         x (Int32.to_int y)
let (>>>) x y = Int32.shift_right_logical x (Int32.to_int y)

let (=) = Int32.equal
let (<>)  x y = not (Int32.equal x y)

let (<.)  x y = Int32.unsigned_compare x y <  0
let (<)   x y = Int32.compare          x y <  0
let (<=.) x y = Int32.unsigned_compare x y <= 0
let (<=)  x y = Int32.compare          x y <= 0

let (>.)  x y = Int32.unsigned_compare x y >  0
let (>)   x y = Int32.compare          x y >  0
let (>=.) x y = Int32.unsigned_compare x y >= 0
let (>=)  x y = Int32.compare          x y >= 0

(* integers function *)

let get_interval imm i j =
  let open Int32 in
  let (-) = Int.sub in
  let (+) = Int.add in
  let (<<) x y = Int32.shift_left  x y in
  let (>>) x y = Int32.shift_right x y in
  let mask = lognot (-1l << (i - j + 1)) in
  (imm >> j) & mask

(* hashtable instructions setup *)

let create_tables list get_name =
  let nb_instruction = List.length list in
  let open Hashtbl in
  let insts = create nb_instruction in
  let str   = create nb_instruction in
  List.iter (fun (k, v) ->
    add insts k v;
    add str (get_name v) k) list;
  insts, str

