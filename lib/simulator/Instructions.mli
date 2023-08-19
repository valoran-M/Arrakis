module R_type : sig
  type t = { funct7 : int; funct3: int;
             rs1: int; rs2: int; rd: int; }

  val decode : Int32.t -> t
    (** [decode code] decode R instruction [code] *)

  val execute : t -> Int32.t -> Int32.t -> Int32.t
  (** [execute inst rs1 rs2] execute [inst] with [rs1] [rs2]
      register value *)
end

module I_type : sig
  type t = { funct3: int; rs1: int; imm: Int32.t; rd: int }

  val decode : Int32.t -> t
    (** [decode code] decode I instruction [code] *)

  val execute_arith : t -> Int32.t -> Int32.t
  (** [execute_arith inst rs1] execute arith I [inst]
      with [rs1] register value *)

  val execute_load  : t -> Int32.t -> Memory.t -> Int32.t
  (** [execute_load inst rs1 mem] execute load I [inst]
      with [rs1] register value *)
end

