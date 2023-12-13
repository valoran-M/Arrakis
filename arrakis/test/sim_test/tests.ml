(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Alcotest

let tests : unit Alcotest.test =
   "Simulator", [
      test_case "R_instruction"       `Quick R_instruction.test;
      test_case "R_instruction_mul"   `Quick R_instruction.test_mul;
      test_case "I_instruction_arith" `Quick I_instruction.test_arith;
      test_case "I_instruction_load"  `Quick I_instruction.test_load;
      test_case "I_instruction_jalr"  `Quick I_instruction.test_jalr;
      test_case "S_instruction"       `Quick S_instruction.test;
      test_case "B_instruction"       `Quick B_instruction.test;
      test_case "U_instruction"       `Quick U_instruction.test;
      test_case "J_instruction"       `Quick J_instruction.test;
  ]
