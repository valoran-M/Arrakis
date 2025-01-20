(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open History
open Insts
open Utils

(* Instruction format :
   31                                       12 11         7 6            0
  +-----------------------------------------------------------------------+
  | imm[31:12]                                | rd         | opcode       | U
  +-----------------------------------------------------------------------+
*)

type t = { rdt: int; imm : int32; }

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

let decode code =
  let (>>) = Int.shift_right_logical   in
  let (&&) x y = Int32.to_int (x & y) in
  { rdt = (code && rdt_mask) >> 7; imm = Int32.logand code i20_mask; }

(* Execution ---------------------------------------------------------------- *)

let execute opcode instruction (arch : Arch.Riscv.t) =
  let open  Arch.Cpu in
  let cpu = arch.cpu in
  let ins = decode instruction in
  let lst = get_reg cpu ins.rdt in
  let res =
    match opcode with
    | 0b0110111l -> ins.imm               (* LUI *)
    | 0b0010111l -> get_pc cpu + ins.imm  (* AUIPC *)
    | _ -> assert false     (* opcode in { 0110111; 0010111 } *)
  in
  set_reg cpu ins.rdt res;
  next_pc cpu; Change_Register (ins.rdt, lst)

