(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(* Memory Segment ----------------------------------------------------------- *)

let stack_begin   = 0x7fff_fff0l
let heap_begin    = 0x1000_8000l
let static_being  = 0x1000_0000l
let text_begin    = 0x0000_0000l
