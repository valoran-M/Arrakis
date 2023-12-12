(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Alcotest

let tests : unit Alcotest.test =
  "Assembler", [
      test_case "Lexer" `Quick Lexer.test;
  ]
