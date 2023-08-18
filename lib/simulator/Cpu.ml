module Regs : sig
  type t

  val make : unit -> t

  val get : t -> int -> Int32.t
  val set : t -> int -> Int32.t -> unit

end = struct
  type t = Int32.t Array.t

  let make () = Array.init 31 (fun _ -> Int32.zero)

  let get regs x_reg = 
    if x_reg = 0
    then Int32.zero
    else regs.(x_reg - 1)

  let set regs x_reg value = regs.(x_reg - 1) <- value
end

type t = { mutable pc : Int32.t; regs: Regs.t }

let make addr_start : t = { pc = addr_start; regs = Regs.make () }

(* --------- get and set registers --------- *)

let get_pc  cpu    = cpu.pc
let set_pc  cpu pc = cpu.pc <- pc
let next_pc cpu    = cpu.pc <- Int32.add cpu.pc (Int32.of_int 4)

let get_reg cpu reg       = Regs.get cpu.regs reg
let set_reg cpu reg value = Regs.set cpu.regs reg value

(* ---------  execute instruction  --------- *)

let opcode_mask = Int32.of_int 0b1111111

let exec (instruction : Int32.t) cpu =
  let open Instructions in
  let opcode = Int32.to_int (Int32.logand opcode_mask instruction) in
  match opcode with
  (* R type *)
  | 0b0110011 ->
    let decode = R_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let rs2 = Regs.get cpu.regs decode.rs2 in
    let return = R_type.execute decode rs1 rs2 in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
  (* I type *)
  | 0b0010011 ->
    let decode = I_type.decode instruction in
    let rs1 = Regs.get cpu .regs decode.rs1 in
    let return = I_type.execute_arith decode rs1 in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
  | 0b0000011
  | 0b1100111
  | 0b1110011 -> Printf.printf "opcode I"
  (* S type *)
  | 0b0100011 -> Printf.printf "opcode S"
  (* B type *)
  | 0b1100011 -> Printf.printf "opcode B"
  (* U type *)
  | 0b0110111
  | 0b0010111 -> Printf.printf "opcode U"
  (* J Type *)
  | 0b1101111 -> Printf.printf "opcode J"
  | _ -> Error.opcode_invalide opcode

