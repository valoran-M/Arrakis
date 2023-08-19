open Simulator

let cpu = Cpu.make 0l
let memory = Memory.make ()

let test_arith () =
  (* ADDI x3, x1, 11 *)
  Cpu.set_reg cpu 1 10l;
  Cpu.exec 0b000000001011_00001_000_00011_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "ADDI 11" 21l (Cpu.get_reg cpu 3);
  (* ADDI x3, x1, -1 *)
  Cpu.exec 0b111111111111_00001_000_00011_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "ADDI -1" 9l (Cpu.get_reg cpu 3);
  (* XORI x1, x2, 01101 *)
  Cpu.set_reg cpu 2 0b11011l;
  Cpu.exec 0b000000001101_00010_100_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "XOR" 0b10110l (Cpu.get_reg cpu 1);
  (* ORI x1, x2,101101 *)
  Cpu.set_reg cpu 2 0b100010l;
  Cpu.exec 0b000000101101_00010_110_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "ORI" 0b101111l (Cpu.get_reg cpu 1);
  (* ANDI x1, x2, 11011 *)
  Cpu.set_reg cpu 2 0b11001l;
  Cpu.exec 0b000000011011_00010_111_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "ANDI" 0b11001l (Cpu.get_reg cpu 1);
  (* SLLI x1, x2, 2 *)
  Cpu.set_reg cpu 2 0b11001l;
  Cpu.exec 0b0000000_00010_00010_001_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "SLLI" 0b1100100l (Cpu.get_reg cpu 1);
  (* SRLI x1, x2, 2 *)
  Cpu.set_reg cpu 2 0b11001l;
  Cpu.exec 0b0000000_00010_00010_101_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "SRLI" 0b110l (Cpu.get_reg cpu 1);
  (* SRAI x1, x2, 2 *)
  Cpu.set_reg cpu 2 (-2l);
  Cpu.exec 0b0100000_00010_00010_101_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "SRAI 2" (-1l) (Cpu.get_reg cpu 1);
  (* SLTI x1, x2, 4 *)
  Cpu.set_reg cpu 2 (-2l);
  Cpu.exec 0b000000000100_00010_010_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "SRAI 4" 1l (Cpu.get_reg cpu 1);
  (* SLTI x1, x2, 4 *)
  Cpu.set_reg cpu 2 (6l);
  Cpu.exec 0b000000000100_00010_010_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "SLTI 4 0" 0l (Cpu.get_reg cpu 1);
  (* SLTIU x1, x2, 4 *)
  Cpu.set_reg cpu 2 (-2l);
  Cpu.exec 0b000000000100_00010_011_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "SLTIU 4 1" 0l (Cpu.get_reg cpu 1);
  (* SLTIU x1, x2, 4 *)
  Cpu.set_reg cpu 2 2l;
  Cpu.exec 0b000000000100_00010_011_00001_0010011l cpu memory;
  Alcotest.check Alcotest.int32 "SLTIU 4 0" 1l (Cpu.get_reg cpu 1);

let test_load () =
  Memory.set_byte
