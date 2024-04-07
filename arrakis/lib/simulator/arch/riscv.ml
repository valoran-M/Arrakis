(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

type t = { cpu : Cpu.t; memory : Memory.t }

let init pc_start memory =
  match pc_start with
  | Some pc -> { memory; cpu = Cpu.make pc }
  | None    -> { memory; cpu = Cpu.make Segment.text_begin }

