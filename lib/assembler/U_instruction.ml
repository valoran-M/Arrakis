open Program

let u_instructions = Hashtbl.create 2

let () =
  List.iter (fun (k, v) -> Hashtbl.add u_instructions k v)
    [
  (*  inst   Opcode       str    *)
      LUI,   (0b0110111, "lui"   );
      AUIPC, (0b0010111, "auipc" );
    ]

let harvest_str () =
  let open Hashtbl in
  let u = create (length u_instructions) in
  iter (fun v (_,k) -> add u k v) u_instructions;
  u
