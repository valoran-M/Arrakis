(* 2^32 memory size *)
let size = 0x1_0000_0000

(* Memory Segment *)

let stack_begin   = 0x7fff_fff0
let heap_begin    = 0x1000_8000
let static_being  = 0x1000_0000
let text_begin    = 0x0000_0000

(* Memory Simulator *)

type t = Bytes.t

let make () = Bytes.make size '\x00'

let get_byte memory addr =
  Utils.char_to_int32 (Bytes.get memory (Int32.to_int addr))
let set_byte memory addr value = 
  Bytes.set memory (Int32.to_int addr) (Utils.char_of_int32 value)

let get_int16 memory addr =
  Int32.of_int (Bytes.get_int16_le memory (Int32.to_int addr))
let set_int16 memory addr value =
  Bytes.set_int16_le memory (Int32.to_int addr) (Int32.to_int value)

let get_int32 memory addr = Bytes.get_int32_le memory (Int32.to_int addr)
let set_int32 memory addr = Bytes.set_int32_le memory (Int32.to_int addr)
