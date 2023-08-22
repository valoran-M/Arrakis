open Program

let j_instructions = Hashtbl.create 1

let () =
  List.iter (fun (k, v) -> Hashtbl.add j_instructions k v)
    [
  (*  inst   Opcode      str   *)
      JAL,   (0b1101111l, "jal" );
    ]

let str_table =
  let open Hashtbl in
  let j = create (length j_instructions) in
  iter (fun v (_,k) -> add j k v) j_instructions;
  j

let write_in_mem mem addr instruction rd imm =
  let (<<) = Int32.shift_left in
  let (||) = Int32.logor in
  let (opcode, _) = Hashtbl.find j_instructions instruction in
  let imm20    = Utils.get_interval imm 12 12 in
  let imm19_12 = Utils.get_interval imm 10 5  in
  let imm11    = Utils.get_interval imm 11 11 in
  let imm10_1  = Utils.get_interval imm 4  1  in
  let code = (imm20 << 31)    || (imm10_1 << 21) || (imm11 << 20) ||
             (imm19_12 << 12) || (rd << 7) || opcode in
  Simulator.Memory.set_int32 mem addr code

