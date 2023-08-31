%{
  open Program
%}

%token COMMA COLON
%token END_LINE EOF
%token LPAR RPAR
%token <Program.r_instruction>INST_R
%token <Program.i_instruction>INST_I
%token <Program.s_instruction>INST_S
%token <Program.b_instruction>INST_B
%token <Program.u_instruction>INST_U
%token <Program.j_instruction>INST_J
%token <Int32.t> INT
%token <Int32.t> REG
%token <string> IDENT

%start program
%type <Program.program> program

%%

imm:
| l=IDENT { Label(l) }
| i=INT   { Imm(i) }
;

instruction:
| id=INST_R rd=REG COMMA? rs1=REG COMMA? rs2=REG
  { R(id, rd, rs1, rs2) }
| id=INST_I rd=REG COMMA? rs1=REG COMMA? simm=imm
  { I(id,rd,rs1,simm) }
| id=INST_S rs2=REG COMMA? rs1=REG COMMA? simm=imm
  { S(id,rs2,rs1,simm) }
| id=INST_B rs1=REG COMMA? rs2=REG COMMA? simm=imm
  { B(id, rs1, rs2, simm) }
| id=INST_U rd=REG COMMA? simm=imm
  { U(id, rd, simm) }
| id=INST_J rd=REG COMMA? simm=imm
  { J(id, rd, simm) }
;

program_line:
| i=instruction END_LINE
  { Instr(0, "", i) }
| i=IDENT COLON { Label(i) }
;

program:
| program_line* EOF
  { $1 }
;
