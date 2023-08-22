let cpu = Simulator.Cpu.make 0l

let memory = Simulator.Memory.make ()

let code = {|
  addi x1 x0 10
|}

let code =
  Assembler.Lexer.prog 0 (Lexing.from_string code)

let () = match code with
  | Seq (Instr (l, I (ADDI, _, _, _)), Nil) -> Printf.printf "%d\n" l
  | _ -> assert false
