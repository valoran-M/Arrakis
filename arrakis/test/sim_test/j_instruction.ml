(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Simulator

let cpu = Cpu.make 0l
let mem = Memory.make ()

let test () =

  (* JAL *)
  Cpu.set_pc cpu 8l;
  ignore (Execute.exec 0b0_0101010101_1_10101010_00001_1101111l cpu mem);
  Alcotest.check Alcotest.int32 "JAL x1" 12l (Cpu.get_reg cpu 1);
  Alcotest.check Alcotest.int32 "JAL pc" 699058l (Cpu.get_pc cpu)
