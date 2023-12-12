(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

let get_interval imm i j =
  let open Int32 in
  let (<<) = shift_left in
  let (>>) = shift_right_logical in
  let (&&) = logand in
  let mask = lognot (-1l << (i - j + 1)) in
  (imm >> j) && mask

