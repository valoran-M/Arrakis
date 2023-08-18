module R_type : sig
  type t = { funct7 : int; funct3: int;
             rs1: int; rs2: int; rd: int; }

  val decode : Int32.t -> t
  val execute : t -> Int32.t -> Int32.t -> Int32.t
end

module I_type : sig
  type t = { funct3: int; rs1: int; imm: Int32.t; rd: int }

  val decode : Int32.t -> t
  val execute_arith : t -> Int32.t -> Int32.t
  val execute_load  : t -> Int32.t -> Memory.t -> Int32.t
end

