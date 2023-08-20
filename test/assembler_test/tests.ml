open Alcotest

let tests : unit Alcotest.test list =
  [ "Assembler", [
      test_case "Lexer" `Quick Lexer.test;
  ] ]
