open Alcotest

let tests : unit Alcotest.test list =
  [ "Simulator", 
    [test_case "R_instruction" `Quick R_instruction.test ]]
