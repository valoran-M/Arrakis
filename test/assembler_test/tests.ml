open Alcotest

let tests : unit Alcotest.test =
  "Assembler", [
      test_case "Lexer" `Quick Lexer.test;
  ]
