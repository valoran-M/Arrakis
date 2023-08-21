open Program

let b_instructions = Hashtbl.create 6

let () =
  List.iter (fun (k, v) -> Hashtbl.add b_instructions k v)
    [
  (*  inst   Opcode      funct3 str    *)
      BEQ,   (0b1100011, 0x0,   "beq"  );
      BNE,   (0b1100011, 0x1,   "bne"  );
      BLT,   (0b1100011, 0x4,   "blt"  );
      BGE,   (0b1100011, 0x5,   "bge"  );
      BLTU,  (0b1100011, 0x6,   "bltu"  );
      BGEU,  (0b1100011, 0x7,   "bgeu"  );
    ]

let harvest_str () =
  let open Hashtbl in
  let b = create (length b_instructions) in
  iter (fun v (_,_,k) -> add b k v) b_instructions;
  b
