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

let digit   = ['0'-'9']
let integer = ('-')? digit+

let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit)*
let label = '.'? (ident | '_')*

let inst_b = "beq"  | "bne" | "blt" | "bge" | "bltu" | "bgeu"

let inst_i = "addi" | "xori" | "ori" | "andi" | "slli" | "srli" | "sari"
           | "slti" | "slti" | "lb"   | "lh"  | "lw"   | "lbu"  | "lhu"
           | "jalr" | "ecal" | "ebreak"

let inst_j = "jal"

let inst_r = "add"  | "sub"  | "xor"  | "or"  | "and"  | "sll"    | "srl"
           | "sra"  | "slt"  | "sltu" | "mul" | "mulh" | "mulhsu" | "mulhu"
           | "div"  | "divu" | "rem"  | "remu"

let inst_s = "sb" | "sh" | "sw"

let inst_u = "lui" | "auipc"

rule prog l = parse
  | '\n'
    { new_line lexbuf; prog (l+1) lexbuf }
  | ' ' | '\t'
    { prog l lexbuf }
  | eof
    { Nil }
  | label as lbl ':'
    { Seq(Label lbl, prog l lexbuf) }
  | inst_b as id
    {
      let open Hashtbl in
      let r   = find b_inst id   in
      let rs1 = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let rs2 = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let imm = parse_imm l lexbuf in
      let instr = Instr(l, B(r, rs1, rs2, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_i as id
    {
      let open Hashtbl in
      let r   = find i_inst id   in
      let rd  = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let rs1 = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let imm = parse_imm l lexbuf in
      let instr = Instr(l, I(r, rd, rs1, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_j as id
    {
      let open Hashtbl in
      let r   = find j_inst id   in
      let rd  = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let imm = parse_imm l lexbuf in
      let instr = Instr(l, J(r, rd, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_r as id
    {
      let open Hashtbl in
      let r   = find r_inst id   in
      let rd  = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let rs1 = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let rs2 = parse_reg l lexbuf in
      let instr = Instr(l, R(r, rd, rs1, rs2)) in
      Seq(instr, prog l lexbuf)
    }
  | inst_s as id
    {
      let open Hashtbl in
      let r   = find s_inst id   in
      let rs2 = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let rs1 = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let imm = parse_imm l lexbuf in
      let instr = Instr(l, S(r, rs2, rs1, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | inst_u as id
    {
      let open Hashtbl in
      let r   = find u_inst id   in
      let rd  = parse_reg l lexbuf in
      parse_comma l lexbuf;
      let imm = parse_imm l lexbuf in
      let instr = Instr(l, U(r, rd, imm)) in
      Seq(instr, end_line l lexbuf)
    }
  | _ as c
    { raise (Lexing_error (l, Inst, String.make 1 c)) }

and parse_reg l = parse
  | ' ' | '\t'
    { parse_reg l lexbuf }
  | ident as id
    {
      try Hashtbl.find regs id
      with Not_found -> raise (Lexing_error (l, Register, id))
    }
  | _ as c
    { raise (Lexing_error (l, Register, String.make 1 c)) }

and parse_comma l = parse
  | ','
    { () }
  | ' ' | '\t'
    { parse_comma l lexbuf }
  | _ as c
   { raise (Lexing_error (l, Comma, String.make 1 c)) }

and parse_imm l = parse
  | ' ' | '\t'
    { parse_imm l lexbuf }
  | integer as i
    { Imm(Int32.of_string i) }
  | label as lbl
    { Label(lbl) }
  | _ as c
    { raise (Lexing_error (l, Imm, String.make 1 c)) }

and end_line l = parse
  | ' ' | '\t' { end_line l lexbuf }
  | '\n' { prog (l+1) lexbuf }
  | '#'  {  comment l lexbuf }

and comment l = parse
  | '\n' { prog (l+1) lexbuf }
  | _    { comment l lexbuf  }
