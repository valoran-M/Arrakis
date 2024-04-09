%{
  open Instructions.Insts
  open Program

  let int_list_to_char_list =
    List.map (fun (i, s) ->
      try Char.chr (Global_utils.Integer.int32_to_int i)
      with Invalid_argument _ ->
        raise (Error.Assembler_error
        (0, (Error.Parsing_error (s ^ " is not int [0,255]")))))

  (* [0-9]: *)
  let label_int = Array.make 10 0

  let create_label i =
    let n = label_int.(i) in
    label_int.(i) <- n + 1;
    Format.sprintf "%d-%d" i n

  let label_f i =
    let n = label_int.(i) in
    Format.sprintf "%d-%d" i n

  let label_b i =
    let n = label_int.(i) in
    Format.sprintf "%d-%d" i (n - 1)

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
/* Pseudo instructions */
%token <int> NOP LI LA J JALP JR JALRP RET CALL TAIL
%token <int * string * Instructions.Insts.reg_offset>      REGS_OFFSET
%token <int * string * Instructions.Insts.reg_reg_offset>  REGS_REGS_OFFSET
%token <int * string * Instructions.Insts.two_reg>         TWO_REGS

%token <string> STRING
%token <int> LLABEL
%token <int * string> LLABEL_F
%token <int * string> LLABEL_B

%token DATA
%token ZERO
%token TEXT
%token BYTES
%token ASCII
%token ASCIZ
%token WORD
%token <int> GLOBL

%start program
%type <Program.program> program

%%

imm:
| l=IDENT { Label l, l }
| i=INT   { let i, s = i in Imm i, s }
| i=LLABEL_F { Label (label_f (fst i)), snd i }
| i=LLABEL_B { Label (label_b (fst i)), snd i }
;

pseudo_inst:
| line=NOP
  { (line, "nop", NOP) }
| line=LI rdt=REG COMMA imm=imm
  { let imm, simm = imm in
    let rdt, rdts = rdt in
    let str = "li " ^ rdts ^ ", " ^ simm in
    match imm with
    | Label _ -> (line, str, LA (rdt, imm))
    | Imm   _ -> (line, str, LI (rdt, imm)) }
| line=LA rdt=REG COMMA imm=imm
  { let imm, simm = imm in
    let rdt, rdts = rdt in
    let str = "la " ^ rdts ^ ", " ^ simm in
    (line, str, LA (rdt, imm)) }
| line=J offset=imm
  { let imm, simm = offset in
    let str = "j " ^ simm in
    (line, str, J imm) }
| line=JALP imm=imm
  { let imm, simm = imm in
    let str = "jal " ^ simm in
    (line, str, JALP imm) }
| line=JR rgs=REG
  { let rg, rgs = rgs in
    let str = "jr " ^ rgs in
    (line, str, JR rg) }
| line=JALRP rg=REG
  { let rg, rgs = rg in
    let str = "jalr " ^ rgs in
    (line, str, JALRP rg) }
| line=RET
  { (line, "ret", RET) }
| line=CALL offset=imm
  { let imm, simm = offset in
    let str = "call " ^ simm in
    (line, str, CALL imm) }
| line=TAIL imm=imm
  { let imm, simm = imm in
    let str = "tail " ^ simm in
    (line, str, TAIL imm) }
| inst=TWO_REGS rdt=REG COMMA rgs=REG
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rgs, rgss = rgs in
    let str = id ^ " " ^ rdts ^ ", " ^ rgss in
    (line, str, Two_Regs (inst, rdt, rgs)) }
| inst=REGS_OFFSET rgs=REG COMMA imm=imm
  { let line, id, inst = inst in
    let rgs, rgss = rgs in
    let imm, simm = imm in
    let str = id ^ " " ^ rgss ^ ", " ^ simm in
    (line, str, Regs_Offset (inst, rgs, imm)) }
| inst=REGS_REGS_OFFSET rgs=REG COMMA rdt=REG COMMA imm=imm
  { let line, id, inst = inst in
    let rgs, rgss = rgs in
    let rdt, rdts = rdt in
    let imm, simm = imm in
    let str = id ^ " " ^ rgss ^ ", " ^ rdts ^ ", " ^ simm in
    (line, str, Regs_Regs_Offset (inst, rgs, rdt, imm)) }
| inst=INST_I_LOAD rdt=REG COMMA imm=imm
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let imm, simm = imm in
    let str = id ^ " " ^ rdts ^ ", " ^ simm in
    (line, str, LGlob(rdt, imm, inst)) }
| inst=INST_S rdt=REG COMMA imm=imm COMMA rgs=REG
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let imm, simm = imm in
    let rgs, rgss = rgs in
    let str = id ^ " " ^ rdts ^ ", " ^ simm ^ ", " ^ rgss in
    (line, str, SGlob (rdt, imm, rgs, inst)) }

basics_inst:
| inst=INST_R rdt=REG COMMA rg1=REG COMMA rg2=REG
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rg1, rg1s = rg1 in
    let rg2, rg2s = rg2 in
    let str = id ^ " " ^ rdts ^ ", " ^ rg1s ^ ", " ^ rg2s in
    (line, str, R (inst, rdt, rg1, rg2)) }
| inst=INST_I_LOAD rdt=REG COMMA imm=imm COMMA? LPAR rg1=REG RPAR
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rg1, rg1s = rg1 in
    let imm, imms = imm in
    let str = id ^ " " ^ rdts ^ ", " ^ imms ^ "(" ^ rg1s ^ ")" in
    (line, str, I (inst, rdt, rg1, imm)) }
| inst=INST_SYST
  { let line, str, inst = inst in
    (line, str, I (inst, 0l, 0l, Imm 0x0l)) }
| inst=INST_I rdt=REG COMMA rg1=REG COMMA imm=imm
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let rg1, rg1s = rg1 in
    let imm, imms = imm in
    let str = id ^ " " ^ rdts ^ ", " ^ rg1s ^ ", " ^ imms in
    (line, str, I (inst, rdt, rg1, imm)) }
| inst=INST_S rg2=REG COMMA imm=imm LPAR rg1=REG RPAR
  { let line, id, inst = inst in
    let rg2, rg2s = rg2 in
    let rg1, rg1s = rg1 in
    let imm, simm = imm in
    let str = id ^ " " ^ rg2s ^ ", " ^ simm ^ "(" ^ rg1s ^ ")" in
    (line, str, S (inst, rg2, rg1, imm)) }
| inst=INST_B rg1=REG COMMA rg2=REG COMMA imm=imm
  { let line, id, inst = inst in
    let rg1, rg1s = rg1 in
    let rg2, rg2s = rg2 in
    let imm, simm = imm in
    let str = id ^ " " ^ rg1s ^ ", " ^ rg2s ^ ", " ^ simm in
    (line, str, B (inst, rg1, rg2, imm)) }
| inst=INST_U rdt=REG COMMA imm=imm
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let imm, simm = imm in
    let str = id ^ " " ^ rdts ^ ", " ^ simm in
    (line, str, U (inst, rdt, imm)) }
| inst=INST_J rdt=REG COMMA imm=imm
  { let line, id, inst = inst in
    let rdt, rdts = rdt in
    let imm, simm = imm in
    let str = id ^ " " ^ rdts ^ ", " ^ simm in
    (line, str, J (inst, rdt, imm)) }
;

inst_aux:
| i=basics_inst { let line, str, inst = i in Text_Instr  (line, str, inst) }
| i=pseudo_inst { let line, str, inst = i in Text_Pseudo (line, str, inst) }
| l=GLOBL i=IDENT { Text_GLabel (l, i) }
;

inst_line:
| inst=inst_aux END_LINE+ { inst }
| i=IDENT COLON END_LINE* { Text_Label i  }
| i=LLABEL      END_LINE* { Text_Label (create_label i) }
;

(* Data --------------------------------------------------------------------- *)

%inline int_list:
  | li=separated_nonempty_list(COMMA, INT) { li }

data:
| ASCII    s=STRING     { Data_Ascii  s }
| ASCIZ    s=STRING     { Data_Asciz  s }
| ZERO     i=INT        { Data_Zero   (fst i) }
| lg=GLOBL i=IDENT      { Data_GLabel (lg, i) }
| BYTES    li=int_list  { Data_Bytes  (int_list_to_char_list li) }
| WORD     li=int_list  { Data_Word   (List.map fst li) }
;

data_line:
| d=data         END_LINE+ { d  }
| i=IDENT  COLON END_LINE* { Data_Label i }
| i=LLABEL COLON END_LINE* { Data_Label (create_label i) }
;

(* Program ------------------------------------------------------------------ *)

p_aux:
| p=p_aux DATA END_LINE* dl=data_line* { { data = p.data @ dl; text = p.text } }
| p=p_aux TEXT END_LINE* il=inst_line* { { data = p.data; text = p.text @ il } }
| pl=inst_line*                        { { data = []; text = pl } }
;

program:
| END_LINE* p=p_aux EOF { p }
;

