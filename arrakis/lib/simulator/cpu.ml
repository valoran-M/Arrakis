module Regs : sig
  type t

  val make : unit -> t

  val get : t -> int -> int32
  val set : t -> int -> int32 -> unit

end = struct
  type t = int32 Array.t

  let make () = Array.init 31 (fun _ -> Int32.zero)

  let get regs x_reg =
    if x_reg = 0
    then Int32.zero
    else regs.(x_reg - 1)

  let set regs x_reg value =
    if 0 < x_reg && x_reg < 32
    then regs.(x_reg - 1) <- value
end

(* Memory Segment *)

type t = { mutable pc : int32; regs: Regs.t }

let make addr_start : t =
  let cpu = { pc = addr_start; regs = Regs.make () } in
  Regs.set cpu.regs 2 Segment.stack_begin;
  Regs.set cpu.regs 3 Segment.static_being;
  cpu

(* ------------------------- get and set registers -------------------------- *)

let get_pc  cpu     = cpu.pc
let set_pc  cpu pc  = cpu.pc <- pc
let next_pc cpu     = cpu.pc <- Int32.add cpu.pc (Int32.of_int 4)
let add_pc  cpu imm = cpu.pc <- Int32.add cpu.pc imm

let get_reg cpu reg       = Regs.get cpu.regs reg
let set_reg cpu reg value = Regs.set cpu.regs reg value

(* -------------------------  execute instruction  -------------------------- *)

let opcode_mask = 0b1111111l

let exec (instruction : Int32.t) cpu memory =
  let open Instructions in
  let opcode = Int32.logand opcode_mask instruction in
  match opcode with
(* R type *)
  | 0b0110011l ->
    let decode = R_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let rs2 = Regs.get cpu.regs decode.rs2 in
    let return = R_type.execute decode rs1 rs2 in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
(* I type *)
  | 0b0010011l ->
    let decode = I_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let return = I_type.execute_arith decode rs1 in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
  | 0b0000011l ->
    let decode = I_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let return = I_type.execute_load decode rs1 memory in
    Regs.set cpu.regs decode.rd return;
    next_pc cpu
  | 0b1100111l ->
    let decode = I_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    (match decode.funct3 with
     | 0x0 ->                                       (* JALR *)
        set_reg cpu decode.rd (Int32.add (get_pc cpu) 4l);
        set_pc cpu (Int32.add rs1 decode.imm)
     | _ -> Error.i_invalid decode.funct3 opcode decode.imm)
  | 0b1110011l -> Printf.printf "opcode I"
(* S type *)
  | 0b0100011l ->
    let decode = S_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let rs2 = Regs.get cpu.regs decode.rs2 in
    S_type.execute decode rs1 rs2 memory;
    next_pc cpu
(* B type *)
  | 0b1100011l ->
    let decode = B_type.decode instruction in
    let rs1 = Regs.get cpu.regs decode.rs1 in
    let rs2 = Regs.get cpu.regs decode.rs2 in
    let imm = B_type.execute decode rs1 rs2 in
    add_pc cpu imm
(* U type *)
  | 0b0110111l ->
    let decode = U_type.decode instruction in
    set_reg cpu decode.rd decode.imm_shift;
    next_pc cpu
  | 0b0010111l ->
    let decode = U_type.decode instruction in
    set_reg cpu decode.rd (Int32.add (get_pc cpu) decode.imm_shift);
    next_pc cpu
(* J Type *)
  | 0b1101111l ->                                    (* JAL *)
    let decode = J_type.decode instruction in
    set_reg cpu decode.rd (Int32.add (get_pc cpu) 4l);
    add_pc cpu decode.imm
  | _ -> Error.opcode_invalid opcode

