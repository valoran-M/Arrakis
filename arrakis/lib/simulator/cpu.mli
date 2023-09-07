type t

val make : int32 -> t
  (** [make pc] make a cpu with [pc] register value *)

val get_pc  : t -> int32
  (** [get_pc cpu] get pc register in [cpu] *)

val set_pc  : t -> int32 -> unit
  (** [set_pc cpu pc] set [pc] register in [cpu] *)

val next_pc : t -> unit
  (** [next_pc cpu] add 4 to the pc register for
      the next instruction*)

val get_reg : t -> int -> int32
  (** [get_reg cpu i] get x[i] register in [cpu] *)

val set_reg : t -> int -> int32 -> unit
  (** [set_reg cpu i val] set [val] in x[i] register *)

exception Syscall

val exec : int32 -> t -> Memory.t -> unit
(** [exec code cpu memory] execute [code] with [cpu] and
    [memory] *)
