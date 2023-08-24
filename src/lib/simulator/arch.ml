type t = { cpu : Cpu.t; memory : Memory.t }

type return = Sys_call | Zero | Continue

let init pc_start memory = { cpu = Cpu.make pc_start; memory }

let exec_instruction arch =
  let code = Memory.get_int32 arch.memory (Cpu.get_pc arch.cpu) in
  if code = 0l
  then Zero
  else (Cpu.exec code arch.cpu arch.memory; Continue)
