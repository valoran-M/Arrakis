type t

val make : Int32.t -> t
  (** [make pc] make a cpu with [pc] register value *)

val get_pc  : t -> Int32.t
  (** [get_pc cpu] get pc register in [cpu] *)

val set_pc  : t -> Int32.t -> unit
  (** [set_pc cpu pc] set [pc] register in [cpu] *)

val next_pc : t -> unit
  (** [next_pc cpu] add 4 to the pc register for
      the next instruction*)

val get_reg : t -> int -> Int32.t
  (** [get_reg cpu i] get x[i] register in [cpu] *)

val set_reg : t -> int -> Int32.t -> unit
  (** [set_reg cpu i val] set [val] in x[i] register *)

val exec : Int32.t -> t -> Memory.t -> unit
(** [exec code cpu memory] execute [code] with [cpu] and
    [memory] *)
