open Program

let s_instructions = Hashtbl.create 3

let () =
  List.iter (fun (k, v) -> Hashtbl.add s_instructions k v)
    [
  (*  inst Opcode       funct3 str  *)
      SB,  (0b0100011,  0x0,   "sb" );
      SH,  (0b0100011,  0x1,   "sh" );
      SW,  (0b0100011,  0x2,   "sw" );
    ]

let harvest_str =
  let open Hashtbl in
  let s = create (length s_instructions) in
  iter (fun v (_,_,k) -> add s k v) s_instructions;
  s
