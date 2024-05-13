%{
  open Gutils.Rope
  open Instructions.Insts
  open Program
  open Format

  let int_list_to_char_list =
    List.map (fun (i, s) ->
      try Char.chr (Gutils.Integer.int32_to_int i)
      with Invalid_argument _ ->
        let open Error in
        raise (Assembler_error (0, (Parsing_error (s ^ " is not in [0,255]")))))

  (* [0-9]: *)
  let label_int = Array.make 10 0

  let create_label i =
    let n = label_int.(i) in
    label_int.(i) <- n + 1;
    sprintf "%d-%d" i n

  let label_f i =
    let n = label_int.(i) in
    sprintf "%d-%d" i n

  let label_b i =
    let n = label_int.(i) in
    sprintf "%d-%d" i (n - 1)

%}

%token COMMA COLON
%token END_LINE EOF
%token LPAR RPAR
%token <int * string * Instructions.Insts.r_instruction> INST_R
%token <int * string * Instructions.Insts.i_instruction> INST_I
%token <int * string * Instructions.Insts.i_instruction> INST_I_LOAD
%token <int * string * Instructions.Insts.i_instruction> INST_SYST
%token <int * string * Instructions.Insts.s_instruction> INST_S
%token <int * string * Instructions.Insts.b_instruction> INST_B
%token <int * string * Instructions.Insts.u_instruction> INST_U
%token <int * string * Instructions.Insts.j_instruction> INST_J
%token <Int32.t * string> INT
%token <Int32.t * string> REG
%token <string> IDENT
(* Pseudo instructions *)
%token <int> NOP LI LA J JALP JR JALRP RET CALL TAIL
%token <int * string * Instructions.Insts.reg_offset>      REGS_OFFSET
%token <int * string * Instructions.Insts.reg_reg_offset>  REGS_REGS_OFFSET
%token <int * string * Instructions.Insts.two_reg>         TWO_REGS

%token <string> STRING
%token <int> LLABEL
%token <int * string> LLABEL_F
%token <int * string> LLABEL_B



(* Static Operation *)
%token LOR LAND
%token BOR BAND BXOR
%token ADD SUB LTE GTE NEQ EQ LT GT
%token MUL DIV REM SHL SHR

%token NOT

%token HI LO

(* Directive *)
%token DATA
%token ZERO
%token TEXT
%token BYTES
%token ASCII
%token ASCIZ
%token WORD
%token <int> GLOBL

%left LOR LAND
%nonassoc NOT
%left ADD SUB  LTE GTE NEQ EQ LT GT
%left BOR BAND BXOR
%left MUL DIV  REM SHL SHR

%start program
%type <Program.t> program

%%

rope(X):
|                 { empty }
| x=X; xs=rope(X) { concat (to_rope x) xs }

%inline exp_list:
| li=separated_nonempty_list(COMMA, expr) { li }

%inline string_list:
| ls=separated_nonempty_list(COMMA, STRING) { ls }

%inline uop:
| SUB { Neg, "-" }
| NOT { Not, "~" }

%inline bop:
| LOR { Lor, "||" } | LAND { Land, "&&" }
| BOR { Bor, "|"  } | BAND { Band, "&"  } | BXOR { Bxor, "^"  }
| ADD { Add, "+"  } | SUB  { Sub,  "-"  } | LTE  { Lte,  "<=" } | GTE { Gte, ">=" }
| NEQ { Neq, "<>" } | EQ   { Eq,   "==" } | LT   { Lt,   "<"  } | GT  { Gt,  ">"  }
| MUL { Mul, "*"  } | DIV  { Div,  "/"  } | REM  { Rem,  "%"  }
| SHL { Shl, ">>" } | SHR  { Shr,  "<<" }

expr:
| l=IDENT              { Lbl l, l }
| i=INT                { Imm (fst i), snd i }
| i=LLABEL_F           { Lbl (label_f (fst i)), snd i }
| i=LLABEL_B           { Lbl (label_b (fst i)), snd i }
| HI LPAR e=expr RPAR  { Hig (fst e), sprintf "%%hi(%s)" (snd e) }
| LO LPAR e=expr RPAR  { Low (fst e), sprintf "%%lo(%s)" (snd e) }
|    LPAR e=expr RPAR  { e }
| uop=uop e=expr       { Uop (fst uop, fst e), sprintf "%s %s" (snd uop) (snd e) }
| e1=expr bop=bop e2=expr
  { Bop (fst bop, fst e1, fst e2),
    sprintf "%s %s %s" (snd e1) (snd bop) (snd e2) }

pseudo_inst:
| line=NOP { line, "nop", NOP }
| line=LI rdt=REG COMMA exp=expr
  { let exp, sexp = exp in
    let rdt, rdts = rdt in
    let str = sprintf "li %s, %s" rdts sexp in
    line, str, LI (rdt, exp) }
| line=LA rdt=REG COMMA exp=expr
  { let exp, sexp = exp in
    let rdt, rdts = rdt in
    let str = sprintf "la %s, %s" rdts sexp in
    line, str, LA (rdt, exp) }
| line=J offset=expr
  { let exp, sexp = offset in
    let str = sprintf "j %s" sexp in
    line, str, J exp }
| line=JALP exp=expr
  { let exp, sexp = exp in
    let str = sprintf "jal %s" sexp in
    line, str, JALP exp }
| line=JR rgs=REG
  { let rg, rgs = rgs in
    let str = sprintf "jr %s" rgs in
    line, str, JR rg }
| line=JALRP rg=REG
  { let rg, rgs = rg in
    let str = sprintf "jalr %s" rgs in
    line, str, JALRP rg }
| line=RET
  { (line, "ret", RET) }
| line=CALL offset=expr
  { let exp, sexp = offset in
    let str = sprintf "call %s" sexp in
    line, str, CALL exp }
| line=TAIL exp=expr
  { let exp, sexp = exp in
    let str = sprintf "tail %s" sexp in
    line, str, TAIL exp }
| inst=TWO_REGS rdt=REG COMMA rgs=REG
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rgs, rgss = rgs in
    let str = sprintf "%s %s, %s" id rdts rgss in
    line, str, Two_Regs (inst, rdt, rgs) }
| inst=REGS_OFFSET rgs=REG COMMA exp=expr
  { let line, id, inst = inst in
    let rgs, rgss = rgs in
    let exp, sexp = exp in
    let str = sprintf "%s %s, %s" id rgss sexp in
    line, str, Regs_Offset (inst, rgs, exp) }
| inst=REGS_REGS_OFFSET rgs=REG COMMA rdt=REG COMMA exp=expr
  { let line, id, inst = inst in
    let rgs, rgss = rgs in
    let rdt, rdts = rdt in
    let exp, sexp = exp in
    let str = sprintf "%s %s, %s, %s" id rgss rdts sexp in
    line, str, Regs_Regs_Offset (inst, rgs, rdt, exp) }
| inst=INST_I_LOAD rdt=REG COMMA exp=expr
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let exp, sexp = exp in
    let str = sprintf "%s %s, %s" id rdts sexp in
    line, str, LGlob(rdt, exp, inst) }
| inst=INST_S rdt=REG COMMA exp=expr COMMA rgs=REG
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let exp, sexp = exp in
    let rgs, rgss = rgs in
    let str = sprintf "%s %s, %s, %s" id rdts sexp rgss in
    line, str, SGlob (rdt, exp, rgs, inst) }

basics_inst:
| inst=INST_R rdt=REG COMMA rg1=REG COMMA rg2=REG
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rg1, rg1s = rg1 in
    let rg2, rg2s = rg2 in
    let str = sprintf "%s %s, %s, %s" id rdts rg1s rg2s in
    line, str, R (inst, rdt, rg1, rg2) }
| inst=INST_I_LOAD rdt=REG COMMA exp=expr COMMA? LPAR rg1=REG RPAR
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rg1, rg1s = rg1 in
    let exp, exps = exp in
    let str = sprintf "%s %s, %s(%s)" id rdts exps rg1s in
    line, str, I (inst, rdt, rg1, exp) }
| inst=INST_SYST
  { let line, str, inst = inst in
    line, str, I (inst, 0l, 0l, Imm 0x0l) }
| inst=INST_I rdt=REG COMMA rg1=REG COMMA exp=expr
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rg1, rg1s = rg1 in
    let exp, exps = exp in
    let str = sprintf "%s %s, %s, %s" id rdts rg1s exps in
    line, str, I (inst, rdt, rg1, exp) }
| inst=INST_S rg2=REG COMMA exp=expr LPAR rg1=REG RPAR
  { let line, id, inst = inst in
    let rg2, rg2s = rg2 in
    let rg1, rg1s = rg1 in
    let exp, exps = exp in
    let str = sprintf "%s %s, %s(%s)" id rg2s exps rg1s in
    line, str, S (inst, rg2, rg1, exp) }
| inst=INST_B rg1=REG COMMA rg2=REG COMMA exp=expr
  { let line, id, inst = inst in
    let rg1, rg1s = rg1 in
    let rg2, rg2s = rg2 in
    let exp, exps = exp in
    let str = sprintf "%s %s, %s, %s" id rg1s rg2s exps in
    line, str, B (inst, rg1, rg2, exp) }
| inst=INST_U rdt=REG COMMA exp=expr
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let exp, exps = exp in
    let str = sprintf "%s %s, %s" id rdts exps in
    line, str, U (inst, rdt, exp) }
| inst=INST_J rdt=REG COMMA exp=expr
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let exp, exps = exp in
    let str = sprintf "%s %s, %s" id rdts exps in
    line, str, J (inst, rdt, exp) }

text_aux:
| i=basics_inst   { let line, str, inst = i in Text_Instr  (line, str, inst) }
| i=pseudo_inst   { let line, str, inst = i in Text_Pseudo (line, str, inst) }
| l=GLOBL i=IDENT { Text_GLabel (l, i) }

text_line:
| inst=text_aux END_LINE+ { inst }
| i=IDENT COLON END_LINE* { Text_Label i  }
| i=LLABEL      END_LINE* { Text_Label (create_label i) }

(* Data --------------------------------------------------------------------- *)

data:
| ASCII    s=string_list  { Data_Ascii  s }
| ASCIZ    s=string_list  { Data_Asciz  s }
| ZERO     i=INT          { Data_Zero   (fst i) }
| l=GLOBL  i=IDENT        { Data_GLabel (l, i) }
| BYTES    le=exp_list    { Data_Bytes  (List.map fst le) }
| WORD     le=exp_list    { Data_Word   (List.map fst le) }

data_line:
| d=data         END_LINE+ { d }
| i=IDENT  COLON END_LINE* { Data_Label i }
| i=LLABEL COLON END_LINE* { Data_Label (create_label i) }

(* Program ------------------------------------------------------------------ *)

(* data, text *)
p_aux:
| p=p_aux DATA END_LINE* dl=rope(data_line) { concat (fst p) dl, snd p }
| p=p_aux TEXT END_LINE* tl=rope(text_line) { fst p, concat (snd p) tl }
| tl=rope(text_line)                        { empty, tl }

program:
| END_LINE* p=p_aux EOF { { data = to_list (fst p); text = to_list (snd p) } }
