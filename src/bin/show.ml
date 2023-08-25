open Simulator

exception Break

let ( + ) = Int32.add
let ( * ) = Int32.mul

(* ------------------------------ Instructions ------------------------------ *)

let reg_to_str reg = "x" ^ (Int.to_string reg)

let code_print = 8

let print_code _arch code =
  let opcode = Int32.logand 0b1111111l code in
  match opcode with
  (* R type *)
  | 0b0110011l ->
    let inst= Instructions.R_type.decode code in
    Printf.sprintf "%s, %s, %s"
      (reg_to_str inst.rd) (reg_to_str inst.rs1) (reg_to_str inst.rs2)
  (* I type *)
  | 0b0010011l | 0b0000011l | 0b1100111l | 0b1110011l ->
    let inst = Instructions.I_type.decode code in
    Printf.sprintf "%s, %s, %d"
      (reg_to_str inst.rd) (reg_to_str inst.rs1) (Int32.to_int inst.imm)
  (* S type *)
  | 0b0100011l ->
    let inst = Instructions.S_type.decode code in
    Printf.sprintf "%s, %d(%s)"
      (reg_to_str inst.rs2) (Int32.to_int inst.imm) (reg_to_str inst.rs1)
  (* B type *)
  | 0b1100011l  ->
    let inst = Instructions.B_type.decode code in
    Printf.sprintf "%s, %s, 0x%x"
      (reg_to_str inst.rs1) (reg_to_str inst.rs2) (Int32.to_int inst.imm)
  (* U type *)
  | 0b0110111l | 0b0010111l  ->
    let inst = Instructions.U_type.decode code in
    Printf.sprintf "%d" (Int32.to_int inst.imm_shift)
  (* J type *)
  | 0b1101111l   ->
    let inst = Instructions.J_type.decode code in
    Printf.sprintf "%d" (Int32.to_int inst.imm)
  | _ -> raise Break


let print_prog (arch : Arch.t) debug =
  let pc = Cpu.get_pc arch.cpu in
  let addr = pc in
  let code = Memory.get_int32 arch.memory  addr in
  try
  print_string "   Adress\t\tMachine Code\t\tBasic Code\t\tOriginal Code\n";
  let _, orignal_code = Hashtbl.find debug pc in
  Printf.printf "-> 0x%08x\t\t0x%08x\t\t%s\t\t%s\n" (Int32.to_int addr)
    (Int32.to_int code) (print_code arch code) orignal_code;
  for i=1 to code_print do
    let addr = (pc + Int32.of_int i * 0x4l) in
    let code = Memory.get_int32 arch.memory  addr in
    let _, orignal_code = Hashtbl.find debug addr in
    Printf.printf "   0x%08x\t\t0x%08x\t\t%s\t\t%s\n" (Int32.to_int addr)
      (Int32.to_int code) (print_code arch code) orignal_code
  done
  with Break -> ()
