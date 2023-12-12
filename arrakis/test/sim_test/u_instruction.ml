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

  (* LUI *)
  Cpu.set_pc cpu 8l;
  ignore (Execute.exec 0b00000000000000000001_00001_0110111l cpu mem);
  Alcotest.check Alcotest.int32 "LUI x1" 0b1000000000000l (Cpu.get_reg cpu 1);

  (* AUIPC *)
  Cpu.set_pc cpu 8l;
  ignore (Execute.exec 0b00000000000000000001_00001_0010111l cpu mem);
  Alcotest.check Alcotest.int32 "LUI x1" 0b1000000001000l (Cpu.get_reg cpu 1);
