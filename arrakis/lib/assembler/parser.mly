%{
  open Program
%}

%token COMMA COLON
%token END_LINE EOF
%token LPAR RPAR
%token <Program.r_instruction * int * string> INST_R
%token <Program.i_instruction * int * string> INST_I
%token <Program.s_instruction * int * string> INST_S
%token <Program.b_instruction * int * string> INST_B
%token <Program.u_instruction * int * string> INST_U
%token <Program.j_instruction * int * string> INST_J
%token <Int32.t * string> INT
%token <Int32.t * string> REG
%token <string> IDENT

%start program
%type <Program.program> program

%%

imm:
| l=IDENT { (Label(l), l) }
| i=INT   { let i, s  = i in (Imm(i), s) }
;

instruction:
| inst=INST_R rd=REG COMMA? rs1=REG COMMA? rs2=REG
  { let inst, line, id = inst in
    let (rd,  rds) = rd in
    let (rs1, s1 ) = rs1 in
    let (rs2, s2 ) = rs2 in
    (R(inst, rd, rs1, rs2), line, id ^ " " ^ rds ^ ", " ^ s1 ^ ", " ^ s2) }
| inst=INST_I rd=REG COMMA? rs1=REG COMMA? simm=imm
  { let inst, line, id = inst in
    let (rd,   rds) = rd in
    let (rs1,  s1) = rs1 in
    let (simm, s) = simm in
    (I(inst,rd,rs1,simm), line, id ^ " " ^ rds ^ ", " ^ s1 ^ ", " ^ s) }
| inst=INST_S rs2=REG COMMA? simm=imm LPAR rs1=REG RPAR
  { let inst, line, id = inst in
    let (rs2, s2  ) = rs2 in
    let (rs1, s1  ) = rs1 in
    let (imm, simm) = simm in
    (S(inst, rs2, rs1, imm), line, id ^ " " ^ s2 ^ ", " ^ simm ^ "(" ^ s1 ^ ")") }
| inst=INST_B rs1=REG COMMA? rs2=REG COMMA? simm=imm
  { let inst, line, id = inst in
    let (rs1, s1   ) = rs1 in
    let (rs2, s2   ) = rs2 in
    let (imm, simm ) = simm in
    (B(inst, rs1, rs2, imm), line, id ^ " " ^ s1 ^ ", " ^ s2 ^ ", " ^ simm) }
| inst=INST_U rd=REG COMMA? simm=imm
  { let inst, line, id = inst in
    let (rd,  rds  ) = rd in
    let (imm, simm ) = simm in
    (U(inst, rd, imm), line, id ^ " " ^ rds ^ ", " ^ simm) }
| inst=INST_J rd=REG COMMA? simm=imm
  { let inst, line, id = inst in
    let (rd,  rds ) = rd in
    let (imm, simm) = simm in
    (J(inst, rd, imm), line, id ^ " " ^ rds ^ ", " ^ simm) }
;

program_line:
| inst=instruction END_LINE*
  { let i, line, string = inst in
    Instr(line , string, i) }
| i=IDENT COLON { Label(i) }
;

program:
| program_line* EOF
  { $1 }
;
