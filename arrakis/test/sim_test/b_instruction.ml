open Simulator

let cpu = Cpu.make 0l

let mem = Memory.make ()

let test () =
  (* BEQ *)
  Cpu.set_reg cpu 1 3l;
  Cpu.set_reg cpu 2 3l;
  ignore (Execute.exec 0b0000000_00010_00001_000_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BEQ T" 16l (Cpu.get_pc cpu);

  Cpu.set_reg cpu 2 4l;
  ignore (Execute.exec 0b0000000_00010_00001_000_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BEQ F" 20l (Cpu.get_pc cpu);

  (* BNE *)
  ignore (Execute.exec 0b0000000_00010_00001_001_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BNE T" 36l (Cpu.get_pc cpu);
  Cpu.set_reg cpu 1 3l;
  Cpu.set_reg cpu 2 3l;
  ignore (Execute.exec 0b0000000_00010_00001_001_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BNE F" 40l (Cpu.get_pc cpu);

  (* BLT *)
  Cpu.set_reg cpu 1 3l;
  Cpu.set_reg cpu 2 6l;
  ignore (Execute.exec 0b0000000_00010_00001_100_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BLT T" 56l (Cpu.get_pc cpu);
  Cpu.set_reg cpu 2 3l;
  ignore (Execute.exec 0b0000000_00010_00001_100_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BLT F" 60l (Cpu.get_pc cpu);

  (* BGE *)
  Cpu.set_reg cpu 1 6l;
  Cpu.set_reg cpu 2 6l;
  ignore (Execute.exec 0b0000000_00010_00001_101_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BGE T" 76l (Cpu.get_pc cpu);
  Cpu.set_reg cpu 1 1l;
  ignore (Execute.exec 0b0000000_00010_00001_101_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BGE F" 80l (Cpu.get_pc cpu);

  (* BLTU *)
  Cpu.set_reg cpu 1 3l;
  Cpu.set_reg cpu 2 (-1l);
  ignore (Execute.exec 0b0000000_00010_00001_110_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BLTU T" 96l (Cpu.get_pc cpu);
  Cpu.set_reg cpu 2 2l;
  ignore (Execute.exec 0b0000000_00010_00001_110_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BLTU F" 100l (Cpu.get_pc cpu);

  (* BGEU *)
  Cpu.set_reg cpu 1 (-1l);
  Cpu.set_reg cpu 2 5l;
  ignore (Execute.exec 0b0000000_00010_00001_111_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BGEU T" 116l (Cpu.get_pc cpu);
  Cpu.set_reg cpu 1 2l;
  ignore (Execute.exec 0b0000000_00010_00001_111_10000_1100011l cpu mem);
  Alcotest.check Alcotest.int32 "BGEU F" 120l (Cpu.get_pc cpu)
