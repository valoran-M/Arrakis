(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

module Regs : sig
  type t

  val make : unit -> t

  val get : t -> int -> int32
  val set : t -> int -> int32 -> unit

end = struct
  type t = int32 Array.t

  let make () = Array.init 31 (fun _ -> Int32.zero)

  let get regs x_reg =
    if x_reg = 0
    then Int32.zero
    else regs.(x_reg - 1)

  let set regs x_reg value =
    if 0 < x_reg && x_reg < 32
    then regs.(x_reg - 1) <- value
end

(* Memory Segment *)

type t = { mutable pc : int32; regs: Regs.t }

let make addr_start : t =
  let cpu = { pc = addr_start; regs = Regs.make () } in
  Regs.set cpu.regs 2 Segment.stack_begin;
  Regs.set cpu.regs 3 Segment.data_begin;
  cpu

(* Get and set registers ---------------------------------------------------- *)

let get_pc  cpu     = cpu.pc
let set_pc  cpu pc  = cpu.pc <- pc
let next_pc cpu     = cpu.pc <- Int32.add cpu.pc (Int32.of_int 4)
let add_pc  cpu imm = cpu.pc <- Int32.add cpu.pc imm

let get_reg cpu reg       = Regs.get cpu.regs reg
let set_reg cpu reg value = Regs.set cpu.regs reg value

