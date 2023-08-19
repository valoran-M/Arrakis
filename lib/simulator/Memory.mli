type t

val make : unit -> t
  (** Creates a memory area of size 2**32 *)


val get_byte : t -> Int32.t -> Int32.t
  (** [get_byte memory addr] Get Byte in [memory] 
      at the addresse [addr] *)

val set_byte : t -> Int32.t -> Int32.t -> unit
  (** [set_byte memory addr value] Set [value] (Byte size)
      in [memory] at the addresse [addr] *)


val get_int16 : t -> Int32.t -> Int32.t
  (** [get_int16 memory addr] Get int16 in [memory] 
      at the addresse [addr] *)

val set_int16 : t -> Int32.t -> Int32.t -> unit
  (** [set_int16 memory addr value] Set [value] (2 Byte size)
      in [memory] at the addresse [addr] *)


val get_int32 : t -> Int32.t -> Int32.t
  (** [get_int32 memory addr] Get int32 in [memory] 
      at the addresse [addr] *)

val set_int32 : t -> Int32.t -> Int32.t -> unit
  (** [set_int32 memory addr value] Set [value] (4 Byte size)
      in [memory] at the addresse [addr] *)
