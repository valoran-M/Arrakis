(* 2^32 memory size *)
let size = 0x1_0000_0000

(* Memory Simulator --------------------------------------------------------- *)

type t = Bytes.t

let make () = Bytes.create size

let direct_access x = x

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

let get_str memory adr =
  let (+) = Int32.add in
  let c   = ref (get_byte memory adr) in
  let endadr = ref adr in
  while (!c <> 0l) do
    endadr := !endadr + 1l;
    c := get_byte memory !endadr;
  done;
  let adr     = Int32.to_int adr     in
  let endadr  = Int32.to_int !endadr in
  Bytes.to_string (Bytes.sub memory adr endadr)

let set_str memory adr value size  =
  Bytes.blit_string value 0 memory (Utils.int32_to_int adr) size
