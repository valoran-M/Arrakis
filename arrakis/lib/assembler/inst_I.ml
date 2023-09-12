open Error
open Simulator
open Program

let i_instructions = Hashtbl.create 17

let () =
  List.iter (fun (k, v) -> Hashtbl.add i_instructions k v)
    [
  (*  inst    Opcode        funct3  str      *)
      ADDI,   (0b0010011l,  0x0l,   "addi"   );
      XORI,   (0b0010011l,  0x4l,   "xori"   );
      ORI,    (0b0010011l,  0x6l,   "ori"    );
      ANDI,   (0b0010011l,  0x7l,   "andi"   );
      SLLI,   (0b0010011l,  0x1l,   "slli"   );
      SRLI,   (0b0010011l,  0x5l,   "srli"   );
      SARI,   (0b0010011l,  0x5l,   "sari"   );
      SLTI,   (0b0010011l,  0x2l,   "slti"   );
      SLTIU,  (0b0010011l,  0x3l,   "sltiu"  );
      LB,     (0b0000011l,  0x0l,   "lb"     );
      LH,     (0b0000011l,  0x1l,   "lh"     );
      LW,     (0b0000011l,  0x2l,   "lw"     );
      LBU,    (0b0000011l,  0x4l,   "lbu"    );
      LHU,    (0b0000011l,  0x5l,   "lhu"    );
      JALR,   (0b1100111l,  0x0l,   "jalr"   );
      ECALL,  (0b1110011l,  0x0l,   "ecall"  );
      (* EBREAK, (0b1110011l,  0x0l,   "ebreak" ); *)
    ]

let str_table =
  let open Hashtbl in
  let i = create (length i_instructions) in
  iter (fun v (_,_,k) -> add i k v) i_instructions;
  i

let write_in_memory mem addr instruction rd rs1 imm line =
  let (&)     = Int32.logand in
  let (<) x y = Int32.compare x y <  0 in
  if 4096l < (imm & 0b11111111111l)
  then raise (Assembler_error (line, Interval_imm (imm, -2048l, 2027l)))
  else (
    let (<<) = Int32.shift_left in
    let (||) = Int32.logor in
    let (opcode, funct3, _) = Hashtbl.find i_instructions instruction in
    let code = (imm << 20) || (rs1 << 15) || (funct3 << 12) ||
               (rd << 7) || opcode in
    Memory.set_int32 mem addr code)

