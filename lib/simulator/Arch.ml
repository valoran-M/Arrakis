type t = {cpu : Cpu.t; memory : Memory.t }

let init pc_start memory = { cpu = Cpu.make pc_start; memory }
