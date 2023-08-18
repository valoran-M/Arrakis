module Regs : sig
  type t

  val make : unit -> t

  val get : t -> int -> Int32.t
  val set : t -> int -> Int32.t -> unit

end = struct
  type t = Int32.t Array.t

  let make () = Array.init 31 (fun _ -> Int32.zero)

  let get regs x_reg = if x_reg = 0 then Int32.zero else regs.(x_reg - 1)
  let set regs x_reg value = regs.(x_reg) <- value
end


type t = { mutable pc : int; regs: Regs.t }

let make addr_start : t = { pc = addr_start; regs = Regs.make () }

let get_pc cpu = cpu.pc
let set_pc cpu pc = cpu.pc <- pc

let get_reg cpu reg = Regs.get cpu.regs reg
let set_reg cpu reg value = Regs.set cpu.regs reg value
