(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type t =
  | Invalid_opcode      of int32
  | Invalid_R           of int * int
  | Invalid_I           of int * int32 * int32
  | Invalid_I_arith     of int * int32
  | Invalid_I_load      of int
  | Invalid_S           of int
  | Invalid_B           of int
  | Invalid_U           of int32
                        (* val     min     max  *)
  | Interval_imm        of int32 * int32 * int32
  | Conversion_Failure

exception Instruction_error of t

let opcode_invalid code = raise (Instruction_error (Invalid_opcode code))

let r_invalid funct3 funct4 =
  raise (Instruction_error (Invalid_R (funct3, funct4)))

let i_invalid funct3 opcode imm =
  raise (Instruction_error (Invalid_I (funct3, opcode, imm)))

let i_invalid_arith funct3 imm =
  raise (Instruction_error (Invalid_I_arith (funct3, imm)))

let i_invalid_load funct3 =
  raise (Instruction_error (Invalid_I_load funct3))

let s_invalid funct3 =
  raise (Instruction_error (Invalid_S funct3))

let b_invalid funct3 =
  raise (Instruction_error (Invalid_B funct3))

let u_invalid opcode =
  raise (Instruction_error (Invalid_U opcode))

