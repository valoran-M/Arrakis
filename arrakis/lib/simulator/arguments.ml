(******************************************************************************)
(* copyright 2023-2024 - arrakis contributors                                 *)
(*                                                                            *)
(* this file is part of arrakis, a risc-v simulator.                          *)
(* it is distributed under the cecill 2.1 license <http://www.cecill.info>    *)
(******************************************************************************)

open Arch

(*
  Argument Spec :

    sp   +-addresses-+         +-------------arguments-------------+
    |    |           |         |                                   |
  +---+-----+-----+-----+---+-----+-----+-----+-----+-----+-----+-----+
<-| n | *a1 | ... | *an | 0 |an[0]| ... |an[x]| ... |a1[0]| ... |a1[z]| sp_begin
  +---+-----+-----+-----+---+-----+-----+-----+-----+-----+-----+-----+
                                                                   |
                                                              0x7fff_fff0
*)

let write_arguments (arch : Riscv.t) (_args : string list) =
  let _sp = Cpu.get_reg arch.cpu 0x2 in
  (* TODO *)
  assert false

