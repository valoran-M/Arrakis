{
  open Program
  open Lexing

  exception Lexing_error of string

  let r_inst = Inst_R.harvest_str
  let i_inst = Inst_I.harvest_str
  let s_inst = Inst_S.harvest_str
  let b_inst = Inst_B.harvest_str
  let u_inst = Inst_U.harvest_str
  let j_inst = Inst_J.harvest_str

  let regs = Hashtbl.create 63
  let () =
    List.iteri (fun x i -> Hashtbl.add regs x (Int32.of_int i))
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
    Hashtbl.add "fp" 8l;
    for i = 0 to 31 do
      Hashtbl.add regs ("x" ^ (Int.to_string i)) (Int32.of_int i)
    done

}

let digit   = ['0'-'9']
let integer = ('-')? digit+

let alpha = ['a'-'z']
let ident = alpha (alpha | digit)*
let label = '.' ident

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
  | _
    { Instr(i, parse_inst lexbuf) }

and parse_inst = parse
  | inst_b as id
    {
      let open Hashtbl in
      let r   = find b_inst id   in
      let rs1 = parse_reg lexbuf in
      let rs2 = parse_reg lexbuf in
      let imm = parse_imm lexbuf in
      B(r, rs1, rs2, imm)
    }
  | inst_i as id
    {
      let open Hashtbl in
      let r   = find i_inst id   in
      let rd  = parse_reg lexbuf in
      let rs1 = parse_reg lexbuf in
      let imm = parse_imm lexbuf in
      I(r, rd, rs1, imm)
    }
  | inst_j as id
    {
      let open Hashtbl in
      let r   = find j_inst id   in
      let rd  = parse_reg lexbuf in
      let imm = parse_imm lexbuf in
      J(r, rd, imm)
    }
  | inst_r as id
    {
      let open Hashtbl in
      let r   = find r_inst id    in
      let rd  = parse_reg lexbuf  in
      let rs1 = parse_reg lexbuf  in
      let rs2 = parse_reg lexbuf  in
      R(r, rd, rs1, rs2)
    }
  | inst_s as id
    {
      let open Hashtbl in
      let r   = find s_inst id   in
      let rs2 = parse_reg lexbuf in
      let rs1 = parse_reg lexbuf in
      let imm = parse_imm lexbuf in
      S(r, rs2, rs1, imm)
    }
  | inst_u as id
    {
      let open Hashtbl in
      let r   = find u_inst id   in
      let rd  = parse_reg lexbuf in
      let imm = parse_imm lexbuf in
      U(r, rd, imm)
    }
  | _ as c
    { raise (Lexing_error (String.make 1 c)) }

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

and parse_imm = parse
  | integer as i
    { Imm(Int32.of_string i) }
  | label as lbl
    { Label(lbl) }
  | _ as c
    { raise (Lexing_error (String.make 1 c)) }
