open Simulator

let code = {|
     addi x1, x0, 10 # add
     beq  x0, x0, .A
     addi x2, x0, 20
.A:  addi x2, x0, 30
|}

let mem = Assembler.Translate.translate code

let arch = Arch.init (Assembler.Segment.text_begin) mem

let () =
  Arch.exec_instruction arch;
  Printf.printf "%d\n" (Int32.to_int (Cpu.get_reg arch.cpu 1));
  Arch.exec_instruction arch;
  Arch.exec_instruction arch;
  Printf.printf "%d\n" (Int32.to_int (Cpu.get_reg arch.cpu 2))
