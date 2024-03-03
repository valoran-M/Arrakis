(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Insts
open Utils

let instructions =
    [
  (*  inst   Opcode       str    *)
      LUI,   (0b0110111l, "lui"   );
      AUIPC, (0b0010111l, "auipc" );
    ]

let instructions, str_table = create_tables instructions (fun (_, v) -> v)

(* Code and decode ---------------------------------------------------------- *)

let code instruction rd imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, _) = Hashtbl.find instructions instruction in
  (imm << 12) || (rd << 7) || opcode

