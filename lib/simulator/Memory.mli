type t

val make : unit -> t

val get_byte : t -> Int32.t -> Int32.t
val set_byte : t -> Int32.t -> Int32.t -> unit

val get_int16 : t -> Int32.t -> Int32.t
val set_int16 : t -> Int32.t -> Int32.t -> unit

val get_int32 : t -> Int32.t -> Int32.t
val set_int32 : t -> Int32.t -> Int32.t -> unit
