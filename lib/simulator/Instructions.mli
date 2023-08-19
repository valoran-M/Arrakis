module R_type : sig
  type t = { funct7 : int; funct3: int;
             rs1: int; rs2: int; rd: int; }

  val decode : int32 -> t
    (** [decode code] decode R instruction [code] *)

  val execute : t -> int32 -> int32 -> int32
    (** [execute inst rs1 rs2] execute [inst] with [rs1] [rs2]
        register value *)
end

module I_type : sig
  type t = { funct3: int; rs1: int; imm: int32; rd: int }

  val decode : int32 -> t
    (** [decode code] decode I instruction [code] *)

  val execute_arith : t -> int32 -> int32
    (** [execute_arith inst rs1] execute arith I [inst]
        with [rs1] register value *)

  val execute_load  : t -> int32 -> Memory.t -> int32
    (** [execute_load inst rs1 mem] execute load I [inst]
        with [rs1] register value *)
end

module S_type : sig
  type t = { funct3: int; rs1: int; rs2: int; imm: int32; }

  val decode : int32 -> t
    (** [decode code] decode S instruction [code] *)

  val execute : t -> int32 -> int32 -> Memory.t -> unit
    (** [execute inst rs1 rs2 mem] execute S [inst] with 
        [rs1] [rs2] register value *)
end

module B_type : sig
  type t = { funct3: int; rs1: int; rs2: int; imm: int32; }

  val decode : int32 -> t
    (** [decode code] decode S instruction [code] *)

  val execute : t -> int32 -> int32 -> int32
    (** [execute inst rs1 rs2] execute S [inst] with 
        [rs1] [rs2] register value return value
        added to pc *)
end

module U_type : sig
  type t = { rd: int; imm_shift: int32; }

  val decode : int32 -> t
    (** [decode code] decode U instruction [code] *)

  val execute : t -> int -> int32 -> int32
    (** [execute inst opcode p]c execute U [inst] *)
end
