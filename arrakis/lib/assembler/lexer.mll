{
  open Error
  open Parser

  let line = ref 0

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

(* Numbers ------------------------------------------------------------------ *)
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal = decimal_literal | hex_literal | oct_literal | bin_literal
let integer = ('-')? int_literal+

(* Alphanumerics ------------------------------------------------------------ *)

let space = [' ' '\t']*
let digit = ['0'-'9']*
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit)*
let label = '.'? (ident | '_')*

(* Instructions ------------------------------------------------------------- *)

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

rule token = parse
  | '\n'{ incr line; END_LINE }
  | ',' { COMMA }
  | ':' { COLON }
  | '#' { comment lexbuf }
  | '(' { LPAR }
  | ')' { RPAR }
  | space { token lexbuf }
  | eof   { EOF }
  | integer as i { INT(Int32.of_string i, i) }
  | inst_b as id { INST_B (Hashtbl.find b_inst id, !line, id) }
  | inst_i as id { INST_I (Hashtbl.find i_inst id, !line, id) }
  | inst_j as id { INST_J (Hashtbl.find j_inst id, !line, id) }
  | inst_r as id { INST_R (Hashtbl.find r_inst id, !line, id) }
  | inst_s as id { INST_S (Hashtbl.find s_inst id, !line, id) }
  | inst_u as id { INST_U (Hashtbl.find u_inst id, !line, id) }
  | label as id
    {
      try REG(Hashtbl.find regs id, id)
      with Not_found -> IDENT (id)
    }
  | _ as c
    { raise (Lexing_error (0, Inst, String.make 1 c)) }

and comment = parse
| '\n' { incr line; END_LINE }
| _    { comment lexbuf }
