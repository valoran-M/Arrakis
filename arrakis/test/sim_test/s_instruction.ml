open Simulator

let cpu = Cpu.make 0l
let memory = Memory.make ()

let test () =

  (* SB x1, 10(x0)*)
  Cpu.set_reg cpu 1 30l;
  ignore (Execute.exec 0b0000000_00001_00000_000_01010_0100011l cpu memory);
  Alcotest.check Alcotest.int32 "SB" 30l (Memory.get_byte memory 10l);

  (* SH x1, 10(x0)*)
  Cpu.set_reg cpu 1 0x2303l;
  ignore (Execute.exec 0b0000000_00001_00000_001_01010_0100011l cpu memory);
  Alcotest.check Alcotest.int32 "SH" 0x2303l (Memory.get_int16 memory 10l);

  (* SW x1, 15(x0)*)
  Cpu.set_reg cpu 1 0x2343_2303l;
  ignore (Execute.exec 0b0000000_00001_00000_010_01111_0100011l cpu memory);
  Alcotest.check Alcotest.int32 "SW" 0x2343_2303l (Memory.get_int32 memory 15l)
