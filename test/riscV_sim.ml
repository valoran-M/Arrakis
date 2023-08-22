let () =
  Alcotest.run "Risc V test" 
    [Simulator_test.Tests.tests; Assembler_test.Tests.tests]
