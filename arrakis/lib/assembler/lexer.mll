(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

{
  open Gutils.Integer
  open Error
  open Parser
  open Regs
  open Hashtbl
  open Format

  let line = ref 1

  let r_inst = Instructions.R.str_table
  let i_inst = Instructions.I.str_table
  let s_inst = Instructions.S.str_table
  let b_inst = Instructions.B.str_table
  let u_inst = Instructions.U.str_table
  let j_inst = Instructions.J.str_table

  let trs_inst = Instructions.Pseudo.two_regs_str
  let rof_inst = Instructions.Pseudo.regs_offset_str
  let rro_inst = Instructions.Pseudo.regs_regs_offset_str

  let string_buffer = Buffer.create 256
  let res_stored_string () = Buffer.reset    string_buffer
  let get_stored_string () = Buffer.contents string_buffer
  let store_string_char ch = Buffer.add_char string_buffer ch

  let string_of_char = sprintf "%c"
  let char_string    = sprintf "'%c'"

  let int_of_numeral n = Char.code n - Char.code '0'

}

(* Numbers ------------------------------------------------------------------ *)

let numeral = ['0'-'9']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal = decimal_literal | hex_literal | oct_literal | bin_literal
let integer = ('-')? int_literal

(* Alphanumerics ------------------------------------------------------------ *)

let space = [' ' '\t']*
let digit = ['0'-'9']*
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit)*
let label = '.'? (ident | '_')+

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

let inst_two_regs  = "mv" | "not" | "neg" | "seqz" | "snez" | "sltz" | "sgtz"
let reg_offset     = "beqz" | "bnez" | "blez" | "bgez" | "bltz" | "bgtz"
let reg_reg_offset = "bgt" | "ble" | "bgtu" | "bleu"

rule token = parse
  | '\n'{ incr line; END_LINE }
  | ',' { COMMA }
  | ':' { COLON }
  | '(' { LPAR }
  | ')' { RPAR }
  | '#'  { one_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf; token lexbuf }
  | "'\\" (['\\' '\'' '\"'] as c) "'" { INT (char_to_int32 c, char_string c) }
  | "'"   (_ as c)                "'" { INT (char_to_int32 c, char_string c) }
  | "'\n'"  { incr line; INT (char_to_int32 '\n', "\\n")}
  | "'\\n'" { INT (char_to_int32 '\n', "\\n") }
  | "'\\t'" { INT (char_to_int32 '\t', "\\t") }
  | "'\\r'" { INT (char_to_int32 '\r', "\\r") }
  | eof   { EOF }
  | space { token lexbuf }
  | integer as i { INT (Int32.of_string i, i) }
  (* Assembler directives *)
  | ".globl" | ".global" { GLOBL !line  }
  | ".data"              { DATA   }
  | ".zero"              { ZERO   }
  | ".text"              { TEXT   }
  | ".byte"              { BYTES  }
  | ".word"              { WORD   }
  | ".ascii"             { ASCII  }
  | ".asciz"             { ASCIZ  }
  (* Instructions *)
  | inst_b as inst { INST_B (!line, inst, find b_inst inst) }
  | inst_i as inst { INST_I (!line, inst, find i_inst inst) }
  | inst_j as inst { INST_J (!line, inst, find j_inst inst) }
  | inst_r as inst { INST_R (!line, inst, find r_inst inst) }
  | inst_s as inst { INST_S (!line, inst, find s_inst inst) }
  | inst_u as inst { INST_U (!line, inst, find u_inst inst) }
  | inst_i_load as inst { INST_I_LOAD (!line, inst, find i_inst inst) }
  | inst_syst   as inst { INST_SYST   (!line, inst, find i_inst inst) }
  (* Pseudo instructions *)
  | inst_two_regs  as inst { TWO_REGS         (!line, inst, find trs_inst inst) }
  | reg_offset     as inst { REGS_OFFSET      (!line, inst, find rof_inst inst) }
  | reg_reg_offset as inst { REGS_REGS_OFFSET (!line, inst, find rro_inst inst) }
  | "nop"  { NOP   !line }
  | "li"   { LI    !line }
  | "la"   { LA    !line }
  | "j"    { J     !line }
  | "jal"  { JALP  !line }
  | "jr"   { JR    !line }
  | "jalr" { JALRP !line }
  | "ret"  { RET   !line }
  | "call" { CALL  !line }
  | "tail" { TAIL  !line }
  (* --- *)
  | (numeral as n) "f"  { LLABEL_F (int_of_numeral n, sprintf "%cf" n) }
  | (numeral as n) "b"  { LLABEL_B (int_of_numeral n, sprintf "%cb" n) }
  | (numeral as n) ":"  { LLABEL   (int_of_numeral n) }
  | label as lbl    { try REG (find regs lbl, lbl) with Not_found -> IDENT lbl }
  | '\"'  { str lexbuf;
            let s  = get_stored_string () in
            res_stored_string ();
            STRING s }
  | _ as c { raise (Assembler_error (!line, Lexing_error (string_of_char c))) }

and one_line_comment = parse
  | '\n' { incr line; END_LINE }
  | _    { one_line_comment lexbuf }

and multi_line_comment = parse
  | '\n'  { incr line; multi_line_comment lexbuf }
  | "*/"  { }
  | _     { multi_line_comment lexbuf }

and str = parse
  | '\\' (['\\' '\'' '\"'] as c ) { store_string_char c; str lexbuf }
  | "\\n"  { store_string_char '\n'; str lexbuf }
  | "\\t"  { store_string_char '\t'; str lexbuf }
  | "\\r"  { store_string_char '\r'; str lexbuf }
  | '"'    { }
  | "\n"   { Error.raise_unclosed !line }
  | eof    { Error.raise_unclosed !line }
  | _ as c { store_string_char c; str lexbuf}

