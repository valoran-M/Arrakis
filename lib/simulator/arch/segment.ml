(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

(* Memory Segment ----------------------------------------------------------- *)

let stack_begin = 0x7fff_fff0l
let heap_begin  = 0x1000_8000l
let data_begin  = 0x1000_0000l
let text_begin  = 0x0000_0000l
