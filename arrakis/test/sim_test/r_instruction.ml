(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Simulator

let cpu = Cpu.make 0l
let memory = Memory.make ()

let test () =

  (* ADD x3, x1, x2*)
  Cpu.set_reg cpu 1 10l;
  Cpu.set_reg cpu 2 15l;
  ignore (Execute.exec 0b0000000_00010_00001_000_00011_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "ADD" 25l (Cpu.get_reg cpu 3);

  (* SUB x1, x3, x2 *)
  ignore (Execute.exec 0b0100000_00010_00001_000_00011_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SUB" 10l (Cpu.get_reg cpu 1);

  (* XOR x1, x2, x3*)
  Cpu.set_reg cpu 2 0b11011l;
  Cpu.set_reg cpu 3 0b10110l;
  ignore (Execute.exec 0b0000000_00011_00010_100_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "XOR" 0b01101l (Cpu.get_reg cpu 1);

  (* OR x1, x2, x3*)
  Cpu.set_reg cpu 2 0b11001l;
  Cpu.set_reg cpu 3 0b10010l;
  ignore (Execute.exec 0b0000000_00011_00010_110_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "OR" 0b11011l (Cpu.get_reg cpu 1);

  (* AND x1, x2, x3 *)
  Cpu.set_reg cpu 2 0b11001l;
  Cpu.set_reg cpu 3 0b10011l;
  ignore (Execute.exec 0b0000000_00011_00010_111_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "AND" 0b10001l (Cpu.get_reg cpu 1);

  (* SLL x1, x2, x3 *)
  Cpu.set_reg cpu 2 0b1011l;
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0000000_00011_00010_001_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SLL" 0b101100l (Cpu.get_reg cpu 1);

  (* SRL x1, x2, x3 *)
  Cpu.set_reg cpu 2 0b101110l;
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0000000_00011_00010_101_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SRL" 0b1011l (Cpu.get_reg cpu 1);

  (* SRA x1, x2, x3*)
  Cpu.set_reg cpu 2 (-2l);
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0100000_00011_00010_101_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SRA" (-1l) (Cpu.get_reg cpu 1);

  Cpu.set_reg cpu 2 6l;
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0100000_00011_00010_101_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SRA" 1l (Cpu.get_reg cpu 1);

  (* SLT x1, x2, x3 *)
  Cpu.set_reg cpu 2 (-2l);
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0000000_00011_00010_010_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SLT 1" 1l (Cpu.get_reg cpu 1);

  Cpu.set_reg cpu 2 6l;
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0000000_00011_00010_010_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SLT 0" 0l (Cpu.get_reg cpu 1);

  (* SLTU x1, x2, x3 *)
  Cpu.set_reg cpu 2 (-2l);
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0000000_00011_00010_011_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SLTU 0" 0l (Cpu.get_reg cpu 1);

  Cpu.set_reg cpu 2 1l;
  Cpu.set_reg cpu 3 2l;
  ignore (Execute.exec 0b0000000_00011_00010_011_00001_0110011l cpu memory);
  Alcotest.check Alcotest.int32 "SLTU 1" 1l (Cpu.get_reg cpu 1)

let test_mul () = ()
