open Program

let s_instructions = Hashtbl.create 3

let () =
  List.iter (fun (k, v) -> Hashtbl.add s_instructions k v)
    [
  (*  inst Opcode       funct3 str  *)
      SB,  (0b0100011l,  0x0l,   "sb" );
      SH,  (0b0100011l,  0x1l,   "sh" );
      SW,  (0b0100011l,  0x2l,   "sw" );
    ]

let str_table =
  let open Hashtbl in
  let s = create (length s_instructions) in
  iter (fun v (_,_,k) -> add s k v) s_instructions;
  s

let write_in_memory mem addr instruction rs2 rs1 imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor  in
  let (opcode, funct3, _) = Hashtbl.find s_instructions instruction in
  let imm11_5 = Utils.get_interval imm 11 5 in
  let imm4_0  = Utils.get_interval imm  4 0 in
  let code = (imm11_5 << 25) || (rs2 << 20)   || (rs1 << 15)
          || (funct3 << 12)  || (imm4_0 << 7) || opcode in
  Simulator.Memory.set_int32 mem addr code

