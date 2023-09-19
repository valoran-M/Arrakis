{
  open Error
  open Parser
  open Regs

  let line = ref 1

  let r_inst = Inst_R.str_table
  let i_inst = Inst_I.str_table
  let s_inst = Inst_S.str_table
  let b_inst = Inst_B.str_table
  let u_inst = Inst_U.str_table
  let j_inst = Inst_J.str_table

  let tr_inst  = Inst_Pseudo.two_regs_str
  let ro_inst  = Inst_Pseudo.regs_offset_str
  let rro_inst = Inst_Pseudo.regs_regs_offset_str

  let string_buffer = Buffer.create 256
  let reset_stored_string () = Buffer.reset string_buffer
  let get_stored_string () = Buffer.contents string_buffer
  let store_string_char c = Buffer.add_char string_buffer c

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
           | "sari" | "slti" | "jalr"

let inst_syst = "ecall"

let inst_i_load = "lb" | "lh"  | "lw" | "lbu"  | "lhu"

let inst_j = "jal"

let inst_r = "add"  | "sub"  | "xor"  | "or"  | "and"  | "sll"    | "srl"
           | "sra"  | "slt"  | "sltu" | "mul" | "mulh" | "mulhsu" | "mulhu"
           | "div"  | "divu" | "rem"  | "remu"

let inst_s = "sb" | "sh" | "sw"

let inst_u = "lui" | "auipc"

(* Pseudo instructions ------------------------------------------------------ *)

let inst_two_regs = "mv" | "not" | "neg" | "seqz" | "snez" | "sltz" | "sgtz"
let reg_offset = "beqz" | "bnez" | "blez" | "bgez" | "bltz" | "bgtz"
let reg_reg_offset = "bgt" | "ble" | "bgtu" | "bleu"

rule token = parse
  | '\n'{ incr line; END_LINE }
  | ',' { COMMA }
  | ':' { COLON }
  | '(' { LPAR }
  | ')' { RPAR }
  | '#' { comment lexbuf }
  | '\"'
      {
        str lexbuf;
        let s  = get_stored_string () in
        reset_stored_string ();
        STRING s
      }
  | "'\n'" { incr line; INT(Int32.of_int (Char.code '\n'), "\\n")}
  | "'\\" (['\\' '\'' '\"'] as c) "'" { INT(Int32.of_int (Char.code c), "'" ^ String.make 1 c ^ "'") }
  | "'\\n'" { INT(Int32.of_int (Char.code '\n'), "\\n") }
  | "'\\t'" { INT(Int32.of_int (Char.code '\t'), "\\t") }
  | "'\\r'" { INT(Int32.of_int (Char.code '\r'), "\\r") }
  | "'" (_ as c) "'"  { INT(Int32.of_int (Char.code c), "'" ^ String.make 1 c ^ "'") }
  | eof   { EOF }
  | space { token lexbuf }
  | integer as i { INT(Int32.of_string i, i) }
  (* Assembler directives *)
  | ".globl" | ".global" { GLOBL !line  }
  | ".data"              { DATA   }
  | ".zero"              { ZERO   }
  | ".text"              { TEXT   }
  | ".byte"              { BYTES  }
  | ".word"              { WORD   }
  | ".asciz"             { ASCIZ }
  (* Instructions *)
  | inst_b as inst { INST_B (!line, inst, Hashtbl.find b_inst inst) }
  | inst_i as inst { INST_I (!line, inst, Hashtbl.find i_inst inst) }
  | inst_j as inst { INST_J (!line, inst, Hashtbl.find j_inst inst) }
  | inst_r as inst { INST_R (!line, inst, Hashtbl.find r_inst inst) }
  | inst_s as inst { INST_S (!line, inst, Hashtbl.find s_inst inst) }
  | inst_u as inst { INST_U (!line, inst, Hashtbl.find u_inst inst) }
  | inst_i_load as inst { INST_I_LOAD (!line, inst, Hashtbl.find i_inst inst) }
  | inst_syst   as inst { INST_SYST (!line, inst, Hashtbl.find i_inst inst) }
  (* Pseudo instructions *)
  | inst_two_regs  as inst { TWO_REGS (!line, inst, Hashtbl.find tr_inst inst) }
  | reg_offset     as inst { REGS_OFFSET (!line, inst, Hashtbl.find ro_inst inst) }
  | reg_reg_offset as inst { REGS_REGS_OFFSET (!line, inst, Hashtbl.find rro_inst inst) }
  | "nop"  { NOP   (!line) }
  | "li"   { LI    (!line) }
  | "la"   { LA    (!line) }
  | "j"    { J     (!line) }
  | "jal"  { JALP  (!line) }
  | "jr"   { JR    (!line) }
  | "jalr" { JALRP (!line) }
  | "ret"  { RET   (!line) }
  | "call" { CALL  (!line) }
  | "tail" { TAIL  (!line) }
  (* --- *)
  | label as lbl
    {
      try  REG (Hashtbl.find regs lbl, lbl)
      with Not_found -> IDENT (lbl)
    }
  | _ as c
    { raise (Assembler_error (!line, Lexing_error (String.make 1 c))) }

and comment = parse
  | '\n' { incr line; END_LINE }
  | _    { comment lexbuf }

and str = parse
  | '\\' (['\\' '\'' '\"'] as c ) { store_string_char c; str lexbuf }
  | "\\n"  { store_string_char '\n'; str lexbuf }
  | "\\t"  { store_string_char '\t'; str lexbuf }
  | "\\r"  { store_string_char '\r'; str lexbuf }
  | '\"'   { }
  | "\n"   { Error.raise_unclosed (!line) }
  | eof    { Error.raise_unclosed (!line) }
  | _ as c { store_string_char c; str lexbuf}

