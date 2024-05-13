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

let ( / ) = Int32.div
let ( * ) = Int32.mul
let ( + ) = Int32.add
let ( - ) = Int32.sub

let hi_lo imm = (imm + 0x800l) >> 12, imm & 0b111111111111l

let cmp op n1 n2 = if op n1 n2 then 1l else 0l

let eval_unop (op : unop) n =
  match op with
  | Neg -> Int32.neg n
  | Not -> Int32.lognot n


let eval_binop (op : binop) n1 n2 =
  let open Int32 in
  match op with
  | Shl  -> shift_left  n1 (to_int n2)
  | Shr  -> shift_right n1 (to_int n2)
  | Add  -> add n1 n2
  | Sub  -> sub n1 n2
  | Mul  -> mul n1 n2
  | Div  -> div n1 n2
  | Rem  -> rem n1 n2
  | Bor  -> logor  n1 n2
  | Bxor -> logxor n1 n2
  | Band -> logand n1 n2
  | Lte  -> cmp (fun n1 n2 -> compare n1 n2 <= 0) n1 n2
  | Lt   -> cmp (fun n1 n2 -> compare n1 n2 <  0) n1 n2
  | Gte  -> cmp (fun n1 n2 -> compare n1 n2 >= 0) n1 n2
  | Gt   -> cmp (fun n1 n2 -> compare n1 n2 >  0) n1 n2
  | Eq   -> cmp (fun n1 n2 -> compare n1 n2 =  0) n1 n2
  | Neq  -> cmp (fun n1 n2 -> compare n1 n2 <> 0) n1 n2
  | Lor  -> cmp (fun n1 n2 -> n1 <> 0l || n2 <> 0l) n1 n2
  | Land -> cmp (fun n1 n2 -> n1 <> 0l && n2 <> 0l) n1 n2

let rec expr_to_int32_rel expr labels line addr =
  match expr with
  | Adr   -> addr
  | Imm i -> i
  | Hig e -> fst (hi_lo (expr_to_int32_rel e labels line addr))
  | Low e -> snd (hi_lo (expr_to_int32_rel e labels line addr))
  | Lbl l -> Label.get_address labels l line - addr
  | Uop (op, e)      -> eval_unop op (expr_to_int32_rel e labels line addr)
  | Bop (op, e1, e2) ->
    eval_binop op (expr_to_int32_rel e1 labels line addr)
                  (expr_to_int32_rel e2 labels line addr)

let rec expr_to_int32 expr labels line addr =
  match expr with
  | Adr   -> addr
  | Imm i -> i
  | Hig e -> fst (hi_lo (expr_to_int32 e labels line addr))
  | Low e -> snd (hi_lo (expr_to_int32 e labels line addr))
  | Lbl l -> Label.get_address labels l line
  | Uop (op, e)      -> eval_unop op (expr_to_int32 e labels line addr)
  | Bop (op, e1, e2) ->
    eval_binop op (expr_to_int32 e1 labels line addr)
                  (expr_to_int32 e2 labels line addr)


let expr_to_char expr labels line addr =
  let i = expr_to_int32 expr labels line addr in
  try ignore (Char.chr (Gutils.Integer.int32_to_int i)); i
  with Invalid_argument _ ->
    let open Error in
    raise (Assembler_error
      (line, (Parsing_error (Format.sprintf "%ld is not in [0,255]" i))))

