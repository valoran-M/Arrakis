open Simulator
open Disassembler

exception Break

let ( + ) = Int32.add
let ( * ) = Int32.mul

let print_prog (arch : Arch.t) debug =
  let pc = Cpu.get_pc arch.cpu in
  try
  print_string "   Adress\t\tMachine Code\t\tBasic Code\t\tOriginal Code\n";
  for i=0 to code_print do
    let addr = (pc + Int32.of_int i * 0x4l) in
    let code = Memory.get_int32 arch.memory  addr in

    if code = 0l
    then (print_endline "   End without syscall"; raise Break)
    else let _, orignal_code = Hashtbl.find debug addr in
         Printf.printf "%s 0x%08x\t\t0x%08x\t\t%-24s%s\n"
          (if i = 0 then "->" else "  ") (Utils.int32_to_int addr)
          (Utils.int32_to_int  code) (print_code arch code) orignal_code
  done
  with _ -> ()

