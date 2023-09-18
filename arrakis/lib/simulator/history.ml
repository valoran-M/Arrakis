open Cpu
open Memory

exception History_Empty

type change =
  | Change_Memory_8  of int32 * int32 (* addresse, value *)
  | Change_Memory_16 of int32 * int32 (* addresse, value *)
  | Change_Memory_32 of int32 * int32 (* addresse, value *)
  | Change_Register  of int   * int32 (* register, value *)
  | Change_Nothing

type sim_change = { last_pc : int32; change : change }

type t = sim_change list


let create_history () = []

let add_history last_pc change history =
  { last_pc; change} :: history

let recover_change (change : sim_change) (arch : Arch.t) =
  Cpu.set_pc arch.cpu change.last_pc;
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

