%{
  open Program

  let int_list_to_char_list =
    List.map (fun (i, s) ->
      try Char.chr (Simulator.Utils.int32_to_int i)
      with Invalid_argument _ ->
        raise (Error.Assembler_error
        (0, (Error.Parsing_error (s ^ " is not int [0,255]")))))
%}

%token COMMA COLON
%token END_LINE EOF
%token LPAR RPAR
%token <int * string * Program.r_instruction> INST_R
%token <int * string * Program.i_instruction> INST_I
%token <int * string * Program.i_instruction> INST_I_LOAD
%token <int * string * Program.i_instruction> INST_SYST
%token <int * string * Program.s_instruction> INST_S
%token <int * string * Program.b_instruction> INST_B
%token <int * string * Program.u_instruction> INST_U
%token <int * string * Program.j_instruction> INST_J
%token <Int32.t * string> INT
%token <Int32.t * string> REG
%token <string> IDENT
/* Pseudo instructions */
%token <int> NOP LI LA J JALP JR JALRP RET CALL TAIL
%token <int * string * Program.reg_offset> REGS_OFFSET
%token <int * string * Program.two_reg> TWO_REGS

%token <string> STRING

%token DATA
%token ZERO
%token TEXT
%token BYTES
%token ASCIZ
%token WORD
%token GLOBL

%start program
%type <Program.program> program

%%

imm:
| l=IDENT { (Label(l), l) }
| i=INT   { let i, s  = i in (Imm(i), s) }
;

pseudo_instruction:
| line=NOP
  { (line, "nop", NOP) }
| line=LI rd=REG COMMA? imm=imm
  { let (imm: imm), simm = imm in
    let rd, rds   = rd  in
    let str = "li " ^ rds ^ ", " ^ simm in
    match imm with
    | Label _ -> (line, str, LA(rd, imm))
    | Imm   _ -> (line, str, LI(rd, imm)) }
| line=LA rd=REG COMMA? imm=imm
  { let imm, simm = imm in
    let rd, rds   = rd  in
    let str = "la " ^ rds ^ ", " ^ simm in
    (line, str, LA(rd, imm)) }
| line=J offset=imm
  { let imm, simm = offset in
    let str = "j " ^ simm  in
    (line, str, J(imm)) }
| line=JALP imm=imm
  { let imm, simm = imm     in
    let str = "jal " ^ simm in
    (line, str, JALP(imm)) }
| line=JR rs=REG
  { let rs, s = rs      in
    let str = "jr " ^ s in
    (line, str, JR(rs)) }
| line=JALRP rs=REG
  { let rs, s = rs        in
    let str = "jalr " ^ s in
    (line, str, JALRP(rs)) }
| line=RET
  { (line, "ret", RET) }
| line=CALL offset=imm
  { let imm, simm = offset   in
    let str = "call " ^ simm in
    (line, str, CALL(imm)) }
| line=TAIL imm=imm
  { let imm, simm = imm      in
    let str = "tail " ^ simm in
    (line, str, TAIL(imm)) }
| inst=TWO_REGS rd=REG COMMA? rs=REG
  { let line, id, inst = inst in
    let rd, rds = rd          in
    let rs, s   = rs          in
    let str = id ^ " " ^ rds ^ ", " ^ s in
    (line, str, Two_Regs(inst, rd, rs)) }
| inst=REGS_OFFSET rs=REG COMMA? imm=imm
  { let line, id, inst = inst in
    let rs, s     = rs        in
    let imm, simm = imm       in
    let str = id ^ " " ^ s ^ ", " ^ simm in
    (line, str, Regs_Offset(inst, rs, imm)) }
| inst=INST_I_LOAD rd=REG COMMA? simm=imm
  { let line, id, inst = inst in
    let rd,   rds = rd        in
    let simm, s   = simm      in
    let str = id ^ " " ^ rds ^ ", " ^ s in
    (line, str, LGlob(rd,simm,inst)) }
| inst=INST_S rd=REG COMMA? simm=imm COMMA? rt=REG
  { let line, id, inst = inst in
    let rd,   rds = rd        in
    let simm, s   = simm      in
    let rt,   rts = rt        in
    let str = id ^ " " ^ rds ^ ", " ^ s ^ ", " ^ rts in
    (line, str, SGlob(rd,simm,rt,inst)) }

instruction:
(* R *)
| inst=INST_R rd=REG COMMA? rs1=REG COMMA? rs2=REG
  { let line, id, inst = inst in
    let rd,  rds = rd         in
    let rs1, s1  = rs1        in
    let rs2, s2  = rs2        in
    let str = id ^ " " ^ rds ^ ", " ^ s1 ^ ", " ^ s2 in
    (line, str, R(inst, rd, rs1, rs2)) }

(* I Load *)
| inst=INST_I_LOAD rd=REG COMMA? simm=imm COMMA? LPAR rs1=REG RPAR
  { let line, id, inst = inst in
    let rd,   rds = rd        in
    let rs1,  s1  = rs1       in
    let simm, s   = simm      in
    let str = id ^ " " ^ rds ^ ", " ^ s ^ "(" ^ s1 ^ ")" in
    (line, str, I(inst,rd,rs1,simm)) }

| inst=INST_SYST
  { let line, str, inst = inst in
    match inst with
    | ECALL -> (line, str, I(inst, 0l, 0l, Imm 0x0l))
    | _ -> assert false }

(* I *)
| inst=INST_I rd=REG COMMA? rs1=REG COMMA? simm=imm
  { let line, id, inst = inst in
    let rd,   rds = rd        in
    let rs1,  s1  = rs1       in
    let simm, s   = simm      in
    let str = id ^ " " ^ rds ^ ", " ^ s1 ^ ", " ^ s in
    (line, str, I(inst, rd, rs1, simm)) }

(* S *)
| inst=INST_S rs2=REG COMMA? simm=imm LPAR rs1=REG RPAR
  { let line, id, inst = inst in
    let rs2, s2   = rs2       in
    let rs1, s1   = rs1       in
    let imm, simm = simm      in
    let str = id ^ " " ^ s2 ^ ", " ^ simm ^ "(" ^ s1 ^ ")" in
    (line, str, S(inst, rs2, rs1, imm)) }

(* B *)
| inst=INST_B rs1=REG COMMA? rs2=REG COMMA? simm=imm
  { let line, id, inst = inst in
    let rs1, s1    = rs1      in
    let rs2, s2    = rs2      in
    let imm, simm  = simm     in
    let str = id ^ " " ^ s1 ^ ", " ^ s2 ^ ", " ^ simm in
    (line, str, B(inst, rs1, rs2, imm)) }

(* U *)
| inst=INST_U rd=REG COMMA? simm=imm
  { let line, id, inst = inst in
    let rd,  rds   = rd       in
    let imm, simm  = simm     in
    let str = id ^ " " ^ rds ^ ", " ^ simm in
    (line, str, U(inst, rd, imm)) }

(* J *)
| inst=INST_J rd=REG COMMA? simm=imm
  { let line, id, inst = inst in
    let rd,  rds  = rd        in
    let imm, simm = simm      in
    let str = id ^ " " ^ rds ^ ", " ^ simm in
    (line, str, J(inst, rd, imm)) }
;

program_line:
| inst=instruction END_LINE*
  { let line, str, inst = inst in
    Prog_Instr(line , str, inst) }
| inst=pseudo_instruction END_LINE*
  { let line, str, inst = inst in
    Prog_Pseudo(line, str, inst) }
| GLOBL COLON? i=IDENT  END_LINE+ { Prog_GLabel i }
| i=IDENT COLON END_LINE*         { Prog_Label i  }
;

data_line:
| i=INT                   END_LINE* { Mem_Value (fst i) }
| BYTES   COLON? INT*     END_LINE+ { Mem_Bytes (int_list_to_char_list $3)  }
| ASCIZ   COLON? s=STRING END_LINE+ { Mem_Asciz s  }
| WORD    COLON? INT*     END_LINE+ { Mem_Word (List.map fst $3) }
| ZERO    COLON? i=INT    END_LINE+ { Mem_Zero (fst i)    }
| GLOBL   COLON? i=IDENT  END_LINE+ { Mem_GLabel i  }
| i=IDENT COLON           END_LINE* { Mem_Label  i  }

program:
| END_LINE* DATA COLON? END_LINE* data_line*
            TEXT COLON? END_LINE* program_line* EOF
    { { memory = $5; program = $9 } }
| END_LINE* TEXT COLON? END_LINE* program_line*
            DATA COLON? END_LINE* data_line* EOF
    { { memory = $9; program = $5 } }
| END_LINE* program_line* EOF { { memory = []; program = $2 } }
;
