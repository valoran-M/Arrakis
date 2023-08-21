open Program

let j_instructions = Hashtbl.create 1

let () =
  List.iter (fun (k, v) -> Hashtbl.add j_instructions k v)
    [
  (*  inst   Opcode      str   *)
      JAL,   (0b1101111l, "jal" );
    ]

let harvest_str =
  let open Hashtbl in
  let j = create (length j_instructions) in
  iter (fun v (_,k) -> add j k v) j_instructions;
  j
