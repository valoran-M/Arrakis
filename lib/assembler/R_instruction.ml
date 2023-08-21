open Program
open Simulator

let r_instructions = Hashtbl.create 18

let () =
  List.iter (fun (k, v) -> Hashtbl.add r_instructions k v)
    [
  (*  inst    Opcode       funct3 funct7 str      *)
      ADD,    (0b0110011l, 0x0l,  0x00l, "add"    );
      SUB,    (0b0110011l, 0x0l,  0x20l, "sub"    );
      XOR,    (0b0110011l, 0x4l,  0x00l, "xor"    );
      OR,     (0b0110011l, 0x6l,  0x00l, "or"     );
      AND,    (0b0110011l, 0x7l,  0x00l, "and"    );
      SLL,    (0b0110011l, 0x1l,  0x00l, "sll"    );
      SRL,    (0b0110011l, 0x5l,  0x00l, "srl"    );
      SRA,    (0b0110011l, 0x5l,  0x20l, "sra"    );
      SLT,    (0b0110011l, 0x2l,  0x00l, "slt"    );
      SLTU,   (0b0110101l, 0x3l,  0x00l, "sltu"   );
  (*  RV32M                                       *)
      MUL,    (0b0110011l, 0x0l,  0x01l, "mul"    );
      MULH,   (0b0110011l, 0x1l,  0x01l, "mulh"   );
      MULHSU, (0b0110011l, 0x2l,  0x01l, "mulhsu" );
      MULHU,  (0b0110011l, 0x3l,  0x01l, "mulhu"  );
      DIV,    (0b0110011l, 0x4l,  0x01l, "div"    );
      DIVU,   (0b0110011l, 0x5l,  0x01l, "divu"   );
      REM,    (0b0110011l, 0x6l,  0x01l, "rem"    );
      REMU,   (0b0110011l, 0x7l,  0x01l, "remu"   );
    ]

let harvest_str () =
  let s = Hashtbl.create (Hashtbl.length r_instructions) in
  Hashtbl.iter (fun v (_,_,_,k) -> Hashtbl.add s k v) r_instructions;
  s

let write_in_memory mem addr instruction rd rs1 rs2 =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, funct3, funct7, _) = Hashtbl.find r_instructions instruction in
  let code = (funct7 << 25) || (rs2 << 20) || (rs1 << 15) || (funct3 << 12) ||
             (rd << 7) || opcode in
  Memory.set_int32 mem addr code;
  4l
