(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Global_utils.Integer
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

  Null-terminated strings => an[x] = a1[z] = 0
*)

let write_arguments (arch : Riscv.t) (args : string list) =
  let (-) = Int32.sub in
  let sp = Cpu.get_reg arch.cpu 0x2 in
  let write_one_string (addrs, eaddr) s =
    let size = String.length s in
    let addr = eaddr - (Int32.of_int size) in
    ignore (Memory.set_strz arch.memory addr s size);
    (addr :: addrs, addr - 1l)
  in

  let (addrs, sp) = List.fold_left write_one_string ([], sp) args in
  Memory.set_byte arch.memory sp 0x0l;

  let sp = alignment sp - 0x4l in
  let sp = List.fold_left (fun sp addr ->
    Memory.set_int32 arch.memory sp addr; sp - 4l) sp addrs
  in

  Memory.set_int32 arch.memory sp (Int32.of_int (List.length addrs)) ;
  Cpu.set_reg arch.cpu 0x2 sp

