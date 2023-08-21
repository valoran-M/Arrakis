open Program

let i_instructions = Hashtbl.create 17

let () =
  List.iter (fun (k, v) -> Hashtbl.add i_instructions k v)
    [
  (*  inst    Opcode       funct3 str      *)
      ADDI,   (0b0010011,  0x0,   "addi"   );
      XORI,   (0b0010011,  0x4,   "xori"   );
      ORI,    (0b0010011,  0x6,   "ori"    );
      ANDI,   (0b0010011,  0x7,   "andi"   );
      SLLI,   (0b0010011,  0x1,   "slli"   );
      SRLI,   (0b0010011,  0x5,   "srli"   );
      SARI,   (0b0010011,  0x5,   "sari"   );
      SLTI,   (0b0010011,  0x2,   "slti"   );
      SLTIU,  (0b0010011,  0x3,   "sltiu"  );
      LB,     (0b0000011,  0x0,   "lb"     );
      LH,     (0b0000011,  0x1,   "lh"     );
      LW,     (0b0000011,  0x2,   "lw"     );
      LBU,    (0b0000011,  0x4,   "lbu"    );
      LHU,    (0b0000011,  0x5,   "lhu"    );
      JALR,   (0b1100111,  0x0,   "jalr"   );
      ECALL,  (0b1110011,  0x0,   "ecall"  );
      EBREAK, (0b1110011,  0x0,   "ebreak" );
    ]

let harvest_str =
  let open Hashtbl in
  let i = create (length i_instructions) in
  iter (fun v (_,_,k) -> add i k v) i_instructions;
  i

