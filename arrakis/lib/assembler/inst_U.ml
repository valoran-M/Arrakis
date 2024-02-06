(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Program

let u_instructions = Hashtbl.create 2

let () =
  List.iter (fun (k, v) -> Hashtbl.add u_instructions k v)
    [
  (*  inst   Opcode       str    *)
      LUI,   (0b0110111l, "lui"   );
      AUIPC, (0b0010111l, "auipc" );
    ]

let str_table =
  let open Hashtbl in
  let u = create (length u_instructions) in
  iter (fun v (_,k) -> add u k v) u_instructions;
  u

let write_in_memory mem addr instruction rd imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, _) = Hashtbl.find u_instructions instruction in
  let code = (imm << 12) || (rd << 7) || opcode in
  Arch.Memory.set_int32 mem addr code

