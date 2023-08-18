
let () = print_endline "Hello, World!"

let cpu = Simulator.Cpu.make 0l

let memory = Simulator.Memory.make ()

let () =
  Simulator.Cpu.exec 0b11111100111000001000011110010011l cpu memory;
  Printf.printf "%s" (Int32.to_string (Simulator.Cpu.get_reg cpu 15))
