(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type t = { cpu : Cpu.t; memory : Memory.t }

let init pc_start memory =
  match pc_start with
  | Some pc -> { memory; cpu = Cpu.make pc }
  | None    -> { memory; cpu = Cpu.make Segment.text_begin }

