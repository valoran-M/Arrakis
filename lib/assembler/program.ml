(* ---------------------------- All instructions ---------------------------- *)

type r_instruction =
  | ADD | SUB
  | XOR | OR  | AND
  | SLL | SRL | SRA
  | SLT | SLTU
  (* RISCV M *)
  | MUL    | MULH
  | MULHSU | MULHU
  | DIV    | DIVU
  | REM    | REMU

type i_instruction =
  (* arit *)
  | ADDI
  | XORI | ORI  | ANDI
  | SLLI | SRLI | SARI
  | SLTI | SLTIU
  (* load *)
  | LB  | LH  | LW
  | LBU | LHU
  (* other *)
  | JALR
  | ECALL| EBREAK
  | LI (* Peudo instruction if she add label in imm *)

type s_instruction =
  (* store *)
  | SB | SH | SW

type b_instruction =
  (* branch *)
  | BEQ  | BNE
  | BLT  | BGE
  | BLTU | BGEU

type u_instruction =
  | LUI | AUIPC

type j_instruction =
  | JAL

(* ------------------------------ Program ----------------------------------- *)

type imm =
  | Label of string
  | Imm   of int32

type instruction =
                      (* rd      rs1     rs2 *)
  | R of r_instruction * int32 * int32 * int32
                      (* rd      rs1     imm *)
  | I of i_instruction * int32 * int32 * imm
                      (* rs2     rs1     imm *)
  | S of s_instruction * int32 * int32 * imm
                      (* rs1     rs2     imm *)
  | B of b_instruction * int32 * int32 * imm
                      (* rd      imm *)
  | U of u_instruction * int32 * imm
                      (* rd      imm *)
  | J of j_instruction * int32 * imm

type program_line =
          (* line nb *)
  | Instr of int     * instruction
  | Label of string

type program =
  | Seq   of program_line * program
  | Nil
