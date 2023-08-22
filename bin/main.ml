let code = {| 
     addi x1 x0 10 # add
     beq  x0 x0 .A
     addi x2 x0 20
.A:  addi x2 x0 30
|}

let mem = Assembler.Translate.translate code

let arch = Simulator.Arch.init (Assembler.Segment.text_begin) mem

  open Simulator

let () =
  Simulator.Arch.exec_instruction arch;
  Printf.printf "%d\n" (Int32.to_int (Cpu.get_reg arch.cpu 1));
  Simulator.Arch.exec_instruction arch;
  Simulator.Arch.exec_instruction arch;
  Printf.printf "%d\n" (Int32.to_int (Cpu.get_reg arch.cpu 2))
