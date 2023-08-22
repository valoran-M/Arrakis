let cpu = Simulator.Cpu.make 0l

let memory = Simulator.Memory.make ()

let code = {|
  addi x1 x0 10 # add
  addi x2 x0 20
|}

let code =
  Assembler.Lexer.prog 0 (Lexing.from_string code)

let () = match code with
  | Seq (Instr (l1, I (ADDI, _, _, _)),
    Seq (Instr (l2, I (ADDI, _, _, _)), Nil)) ->
    Printf.printf "%d %d\n" l1 l2;
  | _ -> assert false
