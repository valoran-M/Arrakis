val colors_init : unit -> unit

val get_input_file : unit -> string

val check_root : unit -> unit

val init_syscall :
  unit -> Format.formatter -> Simulator.Arch.t -> Syscall.Types.syscall_ret

