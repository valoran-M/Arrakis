type t = {cpu : Cpu.t; memory : Memory.t }

let init () = { cpu = Cpu.make 0; memory = Memory.make () }
