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
