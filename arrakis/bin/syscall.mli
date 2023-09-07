open Simulator

type syscall_ret = Exit of int | Continue

val syscall : Format.formatter -> Arch.t -> syscall_ret

