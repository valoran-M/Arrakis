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

let get_byte = Bytes.get
let set_byte = Bytes.set

let get_int32 = Bytes.get_int32_le
let set_int32 = Bytes.set_int32_le
