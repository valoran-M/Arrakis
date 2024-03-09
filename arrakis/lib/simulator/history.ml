(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Arch.Cpu
open Arch.Memory

exception History_Empty

type change =
  | Change_Memory_8  of int32 * int32 (* address,  value *)
  | Change_Memory_16 of int32 * int32 (* address,  value *)
  | Change_Memory_32 of int32 * int32 (* address,  value *)
  | Change_Register  of int   * int32 (* register, value *)
  | Change_Nothing

type sim_change = { last_pc : int32; change : change }

type t = sim_change list

let create_history () = []

let add_history last_pc change history =
  { last_pc; change} :: history

let create_write_mem length addr last_value =
  match length with
  | 8  -> Change_Memory_8  (addr, last_value)
  | 16 -> Change_Memory_16 (addr, last_value)
  | 32 -> Change_Memory_32 (addr, last_value)
  | _  -> failwith "not a real length"

let recover_change (change : sim_change) (arch : Arch.Riscv.t) =
  Arch.Cpu.set_pc arch.cpu change.last_pc;
  match change.change with
  | Change_Memory_8  (addr, value) -> set_byte  arch.memory addr value
  | Change_Memory_16 (addr, value) -> set_int16 arch.memory addr value
  | Change_Memory_32 (addr, value) -> set_int32 arch.memory addr value
  | Change_Register  (reg,  value) -> set_reg   arch.cpu    reg  value
  | Change_Nothing                 -> ()

let step_back arch (history : t) =
  match history with
  | []                -> raise History_Empty
  | change :: history -> recover_change change arch; history

let reset arch (history : t) =
  List.iter (fun c -> recover_change c arch) history; []
