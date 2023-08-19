type t =
  | Invalid_opcode of int
  | Invalid_R of int * int
  | Invalid_I_arith of int * Int32.t
  | Invalid_I_load of int
  | Invalid_S of int

exception Simulator_error of t

let opcode_invalid code = raise (Simulator_error (Invalid_opcode code))

let r_invalid funct3 funct4 =
  raise (Simulator_error (Invalid_R (funct3, funct4)))

let i_invalid_arith funct3 imm =
  raise (Simulator_error (Invalid_I_arith (funct3, imm)))

let i_invalid_load funct3 =
  raise (Simulator_error (Invalid_I_load funct3))

let s_invalid funct3 =
  raise (Simulator_error (Invalid_S funct3))
