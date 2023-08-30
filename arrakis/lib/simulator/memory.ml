(* 2^32 memory size *)
let size = 0x1_0000_0000

(* ---------------------------- Memory Simulator ---------------------------- *)

type t = Bytes.t

let make () = Bytes.create size

let get_byte memory addr =
  Int32.of_int (Bytes.get_uint8 memory (Utils.int32_to_int addr))
let set_byte memory addr value = 
  Bytes.set_uint8 memory (Utils.int32_to_int addr) (Utils.int32_to_int value)

let get_int16 memory addr =
  Int32.of_int (Bytes.get_uint16_le memory (Utils.int32_to_int addr))
let set_int16 memory addr value =
  Bytes.set_uint16_le memory (Utils.int32_to_int addr) (Utils.int32_to_int value)

let get_int32 memory addr = Bytes.get_int32_le memory (Utils.int32_to_int addr)
let set_int32 memory addr = Bytes.set_int32_le memory (Utils.int32_to_int addr)

