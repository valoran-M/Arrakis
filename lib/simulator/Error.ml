type t =
  | Invalid_opcode of int
  | Invalid_R_instruction of int * int
  | Invalid_I_arith of int * Int32.t
  | Invalid_I_load of int

exception Simulator_error of t

let opcode_invalide code = raise (Simulator_error (Invalid_opcode code))

let r_invalide funct3 funct4 =
  raise (Simulator_error (Invalid_R_instruction (funct3, funct4)))

let i_invalide_arith funct3 imm =
  raise (Simulator_error (Invalid_I_arith (funct3, imm)))

let i_invalide_load funct3 =
  raise (Simulator_error (Invalid_I_load funct3))
