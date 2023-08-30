{
  open Error
  open Program
  open Lexing

  let r_inst = Inst_R.str_table
  let i_inst = Inst_I.str_table
  let s_inst = Inst_S.str_table
  let b_inst = Inst_B.str_table
  let u_inst = Inst_U.str_table
  let j_inst = Inst_J.str_table

  let regs = Hashtbl.create 63
  let () =
    List.iteri (fun i x -> Hashtbl.add regs x (Int32.of_int i))
    [
      "zero";
      "ra"  ; "sp"  ; "gp"  ; "tp"  ;
      "t0"  ; "t1"  ; "t2"  ;
      "s0"  ; "s1"  ;
      "a0"  ; "a1"  ; "a2"  ; "a3"  ; "a4"  ; "a5"  ; "a6"  ; "a7"  ;
      "s2"  ; "s3"  ; "s4"  ; "s5"  ; "s6"  ; "s7"  ; "s8"  ; "s9"  ;
      "s10" ; "s11" ;
      "t3"  ; "t4"  ; "t5"  ; "t6"  ;
    ];
    Hashtbl.add regs "fp" 8l;
    for i = 0 to 31 do
      Hashtbl.add regs ("x" ^ (Int.to_string i)) (Int32.of_int i)
    done

}

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

let integer = ('-')? int_literal+

let digit = ['0'-'9']*
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit)*
let label = '.'? (ident | '_')*

let inst_b = "beq"  | "bne" | "blt" | "bge" | "bltu" | "bgeu"

let inst_i = "addi" | "xori" | "ori"  | "andi" | "slli" | "srli"
           | "sari" | "slti" | "slti" | "jalr" | "ecal" | "ebreak"

let inst_i_load = "lb" | "lh"  | "lw" | "lbu"  | "lhu"

let inst_j = "jal"

let inst_r = "add"  | "sub"  | "xor"  | "or"  | "and"  | "sll"    | "srl"
           | "sra"  | "slt"  | "sltu" | "mul" | "mulh" | "mulhsu" | "mulhu"
           | "div"  | "divu" | "rem"  | "remu"

let inst_s = "sb" | "sh" | "sw"

let inst_u = "lui" | "auipc"

let space = ' ' | '\t'

rule prog l = parse
  | '\n'
    { new_line lexbuf; prog (l+1) lexbuf }
  | space
    { prog l lexbuf }
  | eof
    { Nil }
  | label as lbl ':'
    { Seq(Label lbl, prog l lexbuf) }
  | inst_b as id
    {
      let open Hashtbl in
      let r   = find b_inst id   in
      let rs1, s1   = parse_reg l lexbuf in
      let rs2, s2   = parse_reg l lexbuf in
      let imm, simm = parse_imm l lexbuf in
      let code  = id ^ " " ^ s1 ^ ", " ^ s2 ^ ", " ^ simm in
      let instr = Instr(l, code, B(r, rs1, rs2, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_i as id
    {
      let open Hashtbl in
      let r   = find i_inst id   in
      let rd,  sd   = parse_reg l lexbuf in
      let rs1, s1   = parse_reg l lexbuf in
      let imm, simm = parse_imm l lexbuf in
      let code  = id ^ " " ^ sd ^ ", " ^ s1 ^ ", " ^ simm in
      let instr = Instr(l, code, I(r, rd, rs1, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_i_load as id
    {
      let open Hashtbl in
      let r   = find i_inst id   in
      let rd,  sd   = parse_reg l lexbuf in
      let imm, simm = parse_imm l lexbuf in
      let rs1, s1   = parse_reg_memory l lexbuf in
      let code  = id ^ " " ^ sd ^ ", " ^ simm ^ "(" ^ s1 ^ ")" in
      let instr = Instr(l, code, I(r, rd, rs1, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_j as id
    {
      let open Hashtbl in
      let r   = find j_inst id   in
      let rd,  sd   = parse_reg l lexbuf in
      let imm, simm = parse_imm l lexbuf in
      let code  = id ^ " " ^ sd ^ ", " ^ simm in
      let instr = Instr(l, code, J(r, rd, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_r as id
    {
      let open Hashtbl in
      let r   = find r_inst id   in
      let rd,  sd = parse_reg l lexbuf in
      let rs1, s1 = parse_reg l lexbuf in
      let rs2, s2 = parse_reg l lexbuf in
      let code  = id ^ " " ^ sd ^ ", " ^ s1 ^ ", " ^ s2 in
      let instr = Instr(l, code, R(r, rd, rs1, rs2)) in
      Seq(instr, prog l lexbuf)
    }
  | inst_s as id
    {
      let open Hashtbl in
      let r   = find s_inst id   in
      let rs2, s2   = parse_reg l lexbuf in
      let imm, simm = parse_imm l lexbuf in
      let rs1, s1   = parse_reg_memory l lexbuf in
      let code  = id ^ " " ^ s2 ^ ", " ^ simm ^ "(" ^ s1 ^ ")" in
      let instr = Instr(l, code, S(r, rs2, rs1, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_u as id
    {
      let open Hashtbl in
      let r   = find u_inst id   in
      let rd,  sd   = parse_reg l lexbuf in
      let imm, simm = parse_imm l lexbuf in
      let code  = id ^ " " ^ sd ^ ", " ^ simm in
      let instr = Instr(l, code, U(r, rd, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | "nop"
    {
      Seq(Pseudo(l,"nop", NOP), end_line l lexbuf)
    }
  | _ as c
    { raise (Lexing_error (l, Inst, String.make 1 c)) }

and parse_reg l = parse
  | space | ','
    { parse_reg l lexbuf }
  | ident as id
    {
      try Hashtbl.find regs id, id
      with Not_found -> raise (Lexing_error (l, Register, id))
    }
  | _ as c
    { raise (Lexing_error (l, Register, String.make 1 c)) }

and parse_reg_memory l = parse
  | space | ','
    { parse_reg l lexbuf }
  | '(' (ident as id) ')'
    {
      try Hashtbl.find regs id, id
      with Not_found -> raise (Lexing_error (l, Register, id))
    }
  | _ as c
    { raise (Lexing_error (l, Register, String.make 1 c)) }


and parse_imm l = parse
  | space | ','
    { parse_imm l lexbuf }
  | integer as i
    { Imm(Int32.of_string i), i }
  | label as lbl
    { Label(lbl), lbl }
  | _ as c
    { raise (Lexing_error (l, Imm, String.make 1 c)) }

and end_line l = parse
  | space { end_line l lexbuf }
  | '\n'  { prog (l+1) lexbuf }
  | '#'   {  comment l lexbuf }

and comment l = parse
  | '\n' { prog (l+1) lexbuf }
  | _    { comment l lexbuf  }
