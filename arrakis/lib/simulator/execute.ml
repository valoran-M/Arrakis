(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Instructions
open Arch

type return =
  | Sys_call of History.t
  | Zero
  | Continue of int32 * History.t

(* Execute instruction  ----------------------------------------------------- *)

let opcode_mask = 0b1111111l

let exec (instruction : Int32.t) (arch : Riscv.t)  =
  let opcode = Int32.logand opcode_mask instruction in
  match opcode with
  (* R type *)
  | 0b0110011l -> R.execute opcode instruction arch
  (* S type *)
  | 0b0100011l -> S.execute opcode instruction arch
  (* B type *)
  | 0b1100011l -> B.execute opcode instruction arch
  (* J Type *)
  | 0b1101111l -> J.execute opcode instruction arch
  (* I type *)
  | 0b0010011l | 0b0000011l
  | 0b1100111l | 0b1110011l -> I.execute opcode instruction arch
  (* U type *)
  | 0b0110111l | 0b0010111l -> U.execute opcode instruction arch
  (* Error *)
  | _ -> Error.opcode_invalid opcode

let exec_instruction (arch : Riscv.t) (history : History.t) =
  let open History in
  let code    = Memory.get_int32 arch.memory (Cpu.get_pc arch.cpu) in
  let last_pc = Cpu.get_pc arch.cpu in

  try
    if code = 0l then Zero
    else
      let last = exec code arch in
      Continue (Cpu.get_pc arch.cpu, add_history last_pc last history)
  with I.Syscall -> Sys_call (add_history last_pc Change_Nothing history)

