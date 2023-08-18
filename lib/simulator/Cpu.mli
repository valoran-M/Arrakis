type t

val make : Int32.t -> t

val get_pc  : t -> Int32.t
val set_pc  : t -> Int32.t -> unit
val next_pc : t -> unit

val get_reg : t -> int -> Int32.t
val set_reg : t -> int -> Int32.t -> unit

val exec : int32 -> t -> Memory.t -> unit
