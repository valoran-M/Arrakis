(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

type 'a t =
  | Nop 
  | Val of 'a
  | Cat of 'a t * 'a t

let empty = Nop

let to_rope i = Val i

let concat l r = Cat (l, r)

let to_list r =
  let rec aux r acc =
    match r with
    | Nop        -> acc
    | Val a      -> a :: acc
    | Cat (l, r) -> aux l (aux r acc)
  in
  aux r []

