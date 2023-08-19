(* -------------------------- Instructions format --------------------------- *)

(* Instruction format :
   31          25 24      20 19      15 14  12 11         7 6            0
  +-----------------------------------------------------------------------+
  | funct7       | rs2      | rs1      |funct3| rd         | opcode       | R
  | imm[11:0]               | rs1      |funct3| rd         | opcode       | I
  | imm[11:5]    | rs2      | rs1      |funct3| imm[4:0]   | opcode       | S
  | imm[12|10:5] | rs2      | rs1      |funct3| imm[4:1|11]| opcode       | B
  | imm[31:12]                                | rd         | opcode       | U
  | imm[20|10:1|11|10:12]                     | rd         | opcode       | J
  +-----------------------------------------------------------------------+
*)

let rd_mask     = Int32.of_int 0b00000000000000000000111110000000
let func3_mask  = Int32.of_int 0b00000000000000000111000000000000
let rs1_mask    = Int32.of_int 0b00000000000011111000000000000000
let rs2_mask    = Int32.of_int 0b00000001111100000000000000000000
let func7_mask  = Int32.of_int 0b11111110000000000000000000000000
let imm12_mask  = Int32.of_int 0b11111111111100000000000000000000
let imm20_mask  = Int32.of_int 0b11111111111111111111000000000000

(* ----------------------------- Int 32 operator ---------------------------- *)


let (-)  = Int32.sub
let (+)  = Int32.add
let (^)  = Int32.logxor
let (||) = Int32.logor
let (&&) = Int32.logand

let (<<) x y = Int32.shift_left x (Int32.to_int y)
let (>>) x y = Int32.shift_right x (Int32.to_int y)
let (>>>) x y = Int32.shift_right_logical x (Int32.to_int y)

let (=) = Int32.equal
let (<.) x y = Int32.unsigned_compare x y < 0
let (<)  x y = Int32.compare          x y < 0

(* ----------------------------- R Instructions ----------------------------- *)

module R_type = struct
  type t = { funct7 : int; funct3: int;
             rs1: int; rs2: int; rd: int; }

  let decode code =
    let (>>) = Int.shift_right_logical in
    let (&&) x y = Int32.to_int (x && y) in
    { 
      funct7 = (func7_mask && code) >> 25;
      funct3 = (func3_mask && code) >> 12;
      rs1 = (rs1_mask && code) >> 15;
      rs2 = (rs2_mask && code) >> 20;
      rd = (rd_mask && code) >> 7;
    }

  let execute instruction rs1 rs2 =
    match instruction.funct3, instruction.funct7 with
    | 0x0, 0x00 -> rs1 +  rs2                   (* ADD   *)
    | 0x0, 0x20 -> rs1 -  rs2                   (* SUB   *)
    | 0x4, 0x00 -> rs1 ^  rs2                   (* XOR   *)
    | 0x6, 0x00 -> rs1 || rs2                   (* OR    *)
    | 0x7, 0x00 -> rs1 && rs2                   (* AND   *)
    | 0x1, 0x00 -> rs1 << rs2                   (* SLL   *)
    | 0x5, 0x00 -> rs1 >>> rs2                  (* SRL   *)
    | 0x5, 0x20 -> rs1 >>  rs2                  (* SRA   *)
    | 0x2, 0x00 -> if rs1 < rs2 then 1l else 0l (* SLT   *)
    | 0x3, 0x00 -> if rs1 <.rs2 then 1l else 0l (* SLTU *)
    | _, _ ->
      Printf.eprintf "%d %d" instruction.funct3 instruction.funct7;
      Error.r_invalide instruction.funct3 instruction.funct7
end

(* ----------------------------- I Instructions ----------------------------- *)

module I_type = struct
  type t = { funct3: int; rs1: int; imm: Int32.t; rd: int }
  
  let decode code =
    let (>>) = Int.shift_right_logical in
    let (&&) x y = Int32.to_int (x && y) in
    {
      funct3 = (func3_mask && code) >> 12;
      rs1 = (rs1_mask && code) >> 15;
      imm = Int32.shift_right_logical (Int32.logand imm12_mask code) 20; 
      rd  = (rd_mask && code) >> 7;
    }

  let execute_arith instruction rs1 =
    let imm = Utils.sign_extended instruction.imm 12 in
    (* if imm[5:11] = 0x20 or 0x00 for shift *)
    let arith = Int32.shift_right_logical imm 5 = 0x20l in
    let logic = Int32.shift_right_logical imm 5 = 0x00l in
    match instruction.funct3 with
    | 0x0 -> rs1 +  imm                           (* ADDI  *)
    | 0x4 -> rs1 ^  imm                           (* XORI  *)
    | 0x6 -> rs1 || imm                           (* ORI   *)
    | 0x7 -> rs1 && imm                           (* ANDI  *)
    | 0x1 when logic -> rs1 <<  imm               (* SLLI  *)
    | 0x5 when logic -> rs1 >>> imm               (* SRLI  *)
    | 0x5 when arith -> rs1 >>  (imm && 0b11111l) (* SRAI  *)
    | 0x2 -> if rs1 < imm then 1l else 0l         (* SLTI  *)
    | 0x3 -> if rs1 <.imm then 1l else 0l         (* SLTIU *)
    | _ -> Error.i_invalide_arith instruction.funct3 instruction.imm

  let execute_load instruction rs1 memory =
    let addr = rs1 + instruction.imm in
    match instruction.funct3 with
    | 0x0 -> Utils.sign_extended (Memory.get_byte memory addr) 8   (* LB  *)
    | 0x1 -> Utils.sign_extended (Memory.get_int16 memory addr) 16 (* LH  *)
    | 0x2 -> Utils.sign_extended (Memory.get_int32 memory addr) 32 (* LW  *)
    | 0x4 -> Memory.get_byte memory addr                           (* LBU *)
    | 0x5 -> Memory.get_int16 memory addr                          (* LHU *)
    | _ -> Error.i_invalide_load instruction.funct3
end

(* ----------------------------- S Instructions ----------------------------- *)

module S_type = struct
  type t = { funct3: int; rs1: int; rs2: int; imm: Int32.t; }

  let decode code =
    let (>>) = Int.shift_right_logical in
    let (&&) x y = Int32.to_int (x && y) in
    {
      funct3 = (code && func3_mask) >> 12;
      rs1 = (code && rs1_mask) >> 15;
      rs2 = (code && rs2_mask) >> 20;
      imm = Int32.logor 
              (Int32.shift_right_logical (Int32.logand code func7_mask) 20)
              (Int32.shift_right_logical (Int32.logand code rd_mask) 7);
    }

let execute instruction rs1 rs2 memory =
  let addr = rs1 + instruction.imm in
  match instruction.funct3 with
    | 0x0 -> Memory.set_byte  memory addr (rs2 && 0b11111111l)
    | 0x1 -> Memory.set_int16 memory addr (rs2 && 0b1111111111111111l)
    | 0x2 -> Memory.set_int32 memory addr rs2
    | _ -> Error.s_invalide instruction.funct3 
end
