open Program

let b_instructions = Hashtbl.create 6

let () =
  List.iter (fun (k, v) -> Hashtbl.add b_instructions k v)
    [
  (*  inst   Opcode       funct3  str    *)
      BEQ,   (0b1100011l, 0x0l,   "beq"  );
      BNE,   (0b1100011l, 0x1l,   "bne"  );
      BLT,   (0b1100011l, 0x4l,   "blt"  );
      BGE,   (0b1100011l, 0x5l,   "bge"  );
      BLTU,  (0b1100011l, 0x6l,   "bltu" );
      BGEU,  (0b1100011l, 0x7l,   "bgeu" );
    ]

let str_table =
  let open Hashtbl in
  let b = create (length b_instructions) in
  iter (fun v (_,_,k) -> add b k v) b_instructions;
  b
