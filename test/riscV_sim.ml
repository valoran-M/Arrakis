let () =
  Alcotest.run "Simulator" Simulator_test.Tests.tests;
  Alcotest.run "Assembler" Assembler_test.Tests.tests
