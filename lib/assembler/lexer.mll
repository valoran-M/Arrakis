{
  open Program
  open Lexing

  exception Lexing_error of string

  let qadd t = List.iter (fun (x, y) -> Hashtbl.add t x y)

  let r_instructions = Hashtbl.create 128
  let () =
    qadd r_instructions
    [
      "add",    ADD;
      "sub",    SUB;
      "xor",    XOR;
      "or",     OR;
      "and",    AND;
      "sll",    SLL;
      "srl",    SRL;
      "sra",    SRA;
      "slt",    SLT;
      "sltu",   SLTU;
      (* RV32M *)
      "mul",    MUL;
      "mulh",   MULH;
      "mulhsu", MULHSU;
      "mulhu",  MULHU;
      "div",    DIV;
      "divu",   DIVU;
      "rem",    REM;
      "remu",   REMU
    ]

  let i_instructions = Hashtbl.create 128
  let () =
    qadd i_instructions
    [
      "addi",   ADDI;
      "xori",   XORI;
      "ori",    ORI;
      "andi",   ANDI;
      "slli",   SLLI;
      "srli",   SRLI;
      "sari",   SARI;
      "slti",   SLTI;
      "sltiu",  SLTIU;
      "lb",     LB;
      "lh",     LH;
      "lw",     LW;
      "lbu",    LBU;
      "lhu",    LHU;
      "jalr",   JALR;
      "ecall",  ECALL;
      "li",     LI
    ]

  let s_instructions = Hashtbl.create 3
  let () =
    qadd s_instructions
    [
      "sb", SB;
      "sh", SH;
      "sw", SW
    ]

  let b_instructions = Hashtbl.create 6
  let () =
    qadd b_instructions
    [
      "beq",  BEQ;
      "bne",  BNE;
      "blt",  BLT;
      "bge",  BGE;
      "bltu", BLTU;
      "bgeu", BGEU
    ]

  let u_instructions = Hashtbl.create 2
  let () =
    qadd u_instructions
    [
      "lui",   LUI;
      "auipc", AUIPC
    ]

  let j_instructions = Hashtbl.create 1
  let () =
    qadd j_instructions
    [
      "jal", JAL;
    ]

  let regs = Hashtbl.create 34
  let () =
    qadd regs
    [
      "zero", 0;
      "ra",   1;
      "sp",   2;
      "gp",   3;
      "tp",   4;
      "t0",   5;
      "t1",   6;
      "t2",   7;
      "fp",   8;
      "s0",   8;
      "s1",   9;
      "a0",   10;
      "a1",   11;
      "a2",   12;
      "a3",   13;
      "a4",   14;
      "a5",   15;
      "a6",   16;
      "a7",   17;
      "s2",   18;
      "s3",   19;
      "s4",   20;
      "s5",   21;
      "s6",   22;
      "s7",   23;
      "s8",   24;
      "s9",   25;
      "s10",  26;
      "s11",  27;
      "t3",   28;
      "t4",   30;
      "t5",   32;
      "t6",   33;
    ]

  let () =
    for i = 0 to 31 do
      Hashtbl.add regs ("x" ^ (Int.to_string i)) i
    done

}

let digit   = ['0'-'9']
let integer = digit+

let alpha = ['a'-'z']
let ident = alpha (alpha | digit)*
let label = '.' ident

rule prog i = parse
  | '\n'
    { new_line lexbuf; prog (i+1) lexbuf }
  | eof
    { Nil }
  | _
    {
      let l = parse_line i lexbuf in
      Seq(l, prog i lexbuf)
    }

and parse_line i = parse
  | ' ' | '\t'
    { parse_line i lexbuf }
  | label as lbl
    { Label lbl }
  | ident as id
    {
      let open Hashtbl in
      let instr =
        if mem r_instructions id then
          (
            let r = find r_instructions id in
            let rd  = parse_reg lexbuf in
            let rs1 = parse_reg lexbuf in
            let rs2 = parse_reg lexbuf in
            R(r, rd, rs1, rs2)
          )
        else if mem i_instructions id then
          (
            let r = find i_instructions id in
            let rd = parse_reg lexbuf in
            let rs1 = parse_reg lexbuf in
            let imm = Imm(0l) in (* TODO *)
            I(r, rd, rs1, imm)
          )
        else if mem s_instructions id then
          (
            let r = find s_instructions id in
            let rs2 = parse_reg lexbuf in
            let rs1 = parse_reg lexbuf in
            let imm = Imm(0l) in (* TODO *)
            S(r, rs2, rs1, imm)
          )
        else if mem b_instructions id then
          (
            let r = find b_instructions id in
            let rs1 = parse_reg lexbuf in
            let rs2 = parse_reg lexbuf in
            let imm = Imm(0l) in (* TODO *)
            B(r, rs1, rs2, imm)
          )
        else if mem u_instructions id then
          (
            let r = find u_instructions id in
            let rd = parse_reg lexbuf in
            let imm = Imm(0l) in (* TODO *)
            U(r, rd, imm)
          )
        else if mem j_instructions id then
          (
            let r = find j_instructions id in
            let rd = parse_reg lexbuf in
            let imm = Imm(0l) in (* TODO *)
            J(r, rd, imm)
          )
        else (raise (Lexing_error id))
      in Instr(i, instr)
    }
  | _ as c
    {
      raise (Lexing_error (String.make 1 c))
    }

and parse_reg = parse
  | ' ' | '\t'
    { parse_reg lexbuf }
  | ident as id
    {
      try Hashtbl.find regs id
      with Not_found -> raise (Lexing_error id)
    }
  | _ as c
    { raise (Lexing_error (String.make 1 c)) }
