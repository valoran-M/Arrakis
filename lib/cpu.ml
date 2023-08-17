module Regs : sig
  type t

  val make : unit -> t

  val get : t -> int -> int
  val set : t -> int -> int -> unit

end = struct
  type t = int Array.t

  let make () = Array.init 31 (fun _ -> 0)

  let get regs i_reg = if i_reg = 0 then 0 else regs.(i_reg - 1)
  let set regs i_reg v = regs.(i_reg) <- v
end


module Cpu = struct
  type t = { pc : int; regs: Regs.t }
  let make addr_start : t = { pc = addr_start; regs = Regs.make () }
end


