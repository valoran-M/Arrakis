open Program
open Simulator

let code = Hashtbl.create 18

let () =
  List.iter (fun (k, v) -> Hashtbl.add code k v)
    [
  (*  inst    Opcode       funct3 funct7 *)
      ADD,    (0b0110011l, 0x0l,  0x00l);
      SUB,    (0b0110011l, 0x0l,  0x20l);
      XOR,    (0b0110011l, 0x4l,  0x00l);
      OR,     (0b0110011l, 0x6l,  0x00l);
      AND,    (0b0110011l, 0x7l,  0x00l);
      SLL,    (0b0110011l, 0x1l,  0x00l);
      SRL,    (0b0110011l, 0x5l,  0x00l);
      SRA,    (0b0110011l, 0x5l,  0x20l);
      SLT,    (0b0110011l, 0x2l,  0x00l);
      SLTU,   (0b0110101l, 0x3l,  0x00l);
      (* mul extension *)
      MUL,    (0b0110011l, 0x0l,  0x01l);
      MULH,   (0b0110011l, 0x1l,  0x01l);
      MULHSU, (0b0110011l, 0x2l,  0x01l);
      MULHU,  (0b0110011l, 0x3l,  0x01l);
      DIV,    (0b0110011l, 0x4l,  0x01l);
      DIVU,   (0b0110011l, 0x5l,  0x01l);
      REM,    (0b0110011l, 0x6l,  0x01l);
      REMU,   (0b0110011l, 0x7l,  0x01l);
    ]

let write_in_memory mem addr instruction rd rs1 rs2 =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, funct3, funct7) = Hashtbl.find code instruction in
  let code = (funct7 << 25) || (rs2 << 20) || (rs1 << 15) || (funct3 << 12) ||
             (rd << 7) || opcode in
  Memory.set_int32 mem addr code;
  4l
  
