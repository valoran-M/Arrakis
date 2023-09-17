module Regs : sig
  type t

  val make : unit -> t

  val get : t -> int -> int32
  val set : t -> int -> int32 -> unit

end

type t = { mutable pc : int32; regs: Regs.t }

val make : int32 -> t
  (** [make pc] make a cpu with [pc] register value *)

val get_pc  : t -> int32
  (** [get_pc cpu] get pc register in [cpu] *)

val set_pc  : t -> int32 -> unit
  (** [set_pc cpu pc] set [pc] register in [cpu] *)

val add_pc : t -> int32 -> unit

val next_pc : t -> unit
  (** [next_pc cpu] add 4 to the pc register for
      the next instruction*)

val get_reg : t -> int -> int32
  (** [get_reg cpu i] get x[i] register in [cpu] *)

val set_reg : t -> int -> int32 -> unit
  (** [set_reg cpu i val] set [val] in x[i] register *)

