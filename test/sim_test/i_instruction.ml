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
  Alcotest.check Alcotest.int32 "SLTIU 4 0" 1l (Cpu.get_reg cpu 1)

let test_load () =
  (* LB *)
  Memory.set_byte memory 10l 10l;
  Cpu.exec 0b000000001010_00000_000_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LB 1" 10l (Cpu.get_reg cpu 1);

  Memory.set_byte memory 10l (0b11111111l);
  Cpu.set_reg cpu 2 5l;
  Cpu.exec 0b000000000101_00010_000_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LB 2" (-1l) (Cpu.get_reg cpu 1);

  (* LH *)
  Memory.set_int16 memory 10l 10l;
  Cpu.exec 0b000000001010_00000_001_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LH 1" 10l (Cpu.get_reg cpu 1);

  Memory.set_int16 memory 10l (0b1111111111111110l);
  Cpu.set_reg cpu 2 5l;
  Cpu.exec 0b000000000101_00010_001_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LH 2" (-2l) (Cpu.get_reg cpu 1);

  (* LW *)
  Memory.set_int32 memory 10l 300l;
  Cpu.exec 0b000000001010_00000_010_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LW 1" 300l (Cpu.get_reg cpu 1);

  Memory.set_int32 memory 13l (-4l);
  Cpu.set_reg cpu 2 8l;
  Cpu.exec 0b000000000101_00010_010_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LW 2" (-4l) (Cpu.get_reg cpu 1);

  (* LBU *)
  Memory.set_byte memory 10l 10l;
  Cpu.exec 0b000000001010_00000_100_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LBU 1" 10l (Cpu.get_reg cpu 1);

  Memory.set_byte memory 10l (0b11111111l);
  Cpu.set_reg cpu 2 5l;
  Cpu.exec 0b000000000101_00010_100_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LBU 2" (0b11111111l) (Cpu.get_reg cpu 1);

  (* LHU *)
  Memory.set_int16 memory 10l 10l;
  Cpu.exec 0b000000001010_00000_101_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LHU 1" 10l (Cpu.get_reg cpu 1);

  Memory.set_int16 memory 20l (0b1111111111111110l);
  Printf.printf "%s\n" (Int32.to_string (Memory.get_int16 memory 20l));
  Cpu.set_reg cpu 2 15l;
  Cpu.exec 0b000000000101_00010_101_00001_0000011l cpu memory;
  Alcotest.check Alcotest.int32 "LHU 2" (0b1111111111111110l) (Cpu.get_reg cpu 1);
