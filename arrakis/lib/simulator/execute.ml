open Cpu

type return = Sys_call | Zero | Continue of int32

(* Execute instruction  ----------------------------------------------------- *)

exception Syscall

let opcode_mask = 0b1111111l

let exec (instruction : Int32.t) (cpu : Cpu.t) memory =
  let open Instructions in
  let opcode = Int32.logand opcode_mask instruction in
  match opcode with
  (* R type *)
  | 0b0110011l ->
    let decode = R_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let rs2 = Regs.get cpu.regs decode.rs2 in
    let return = R_type.execute decode rs1 rs2 in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
  (* I type *)
  | 0b0010011l ->
    let decode = I_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let return = I_type.execute_arith decode rs1 in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
  | 0b0000011l ->
    let decode = I_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let return = I_type.execute_load decode rs1 memory in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
  | 0b1100111l ->
    let decode = I_type.decode instruction in
    let imm    = Utils.sign_extended decode.imm 12 in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    (match decode.funct3 with
     | 0x0 ->                                       (* JALR *)
        set_reg cpu decode.rd (Int32.add (get_pc cpu) 4l);
        set_pc cpu (Int32.add rs1 imm)
     | _ -> Error.i_invalid decode.funct3 opcode decode.imm)
  | 0b1110011l ->
    let decode = I_type.decode instruction in
    if Int32.equal decode.imm 0x0l
    then (next_pc cpu; raise Syscall)
    else Error.i_invalid decode.funct3 opcode decode.imm
  (* S type *)
  | 0b0100011l ->
    let decode = S_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let rs2 = Regs.get cpu.regs decode.rs2 in
    S_type.execute decode rs1 rs2 memory;
    next_pc cpu
  (* B type *)
  | 0b1100011l ->
    let decode = B_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let rs2 = Regs.get cpu.regs decode.rs2 in
    let imm = B_type.execute decode rs1 rs2 in
    add_pc cpu imm
  (* U type *)
  | 0b0110111l ->
    let decode = U_type.decode instruction in
    set_reg cpu decode.rd decode.imm_shift;
    next_pc cpu
  | 0b0010111l ->
    let decode = U_type.decode instruction in
    set_reg cpu decode.rd (Int32.add (get_pc cpu) decode.imm_shift);
    next_pc cpu
  (* J Type *)
  | 0b1101111l ->                                    (* JAL *)
    let decode = J_type.decode instruction in
    set_reg cpu decode.rd (Int32.add (get_pc cpu) 4l);
    add_pc cpu decode.imm
  | _ -> Error.opcode_invalid opcode

let exec_instruction (arch : Arch.t) =
  let code = Memory.get_int32 arch.memory (Cpu.get_pc arch.cpu) in
  if code = 0l
  then Zero
  else (
    try exec code arch.cpu arch.memory; Continue (Cpu.get_pc arch.cpu)
    with Syscall -> Sys_call
  )

