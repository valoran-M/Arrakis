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
    List.iter (fun (x, y) -> Hashtbl.add regs x y)
    [
      "zero", 0l;
      "ra",   1l;
      "sp",   2l;
      "gp",   3l;
      "tp",   4l;
      "t0",   5l;
      "t1",   6l;
      "t2",   7l;
      "fp",   8l;
      "s0",   8l;
      "s1",   9l;
      "a0",   10l;
      "a1",   11l;
      "a2",   12l;
      "a3",   13l;
      "a4",   14l;
      "a5",   15l;
      "a6",   16l;
      "a7",   17l;
      "s2",   18l;
      "s3",   19l;
      "s4",   20l;
      "s5",   21l;
      "s6",   22l;
      "s7",   23l;
      "s8",   24l;
      "s9",   25l;
      "s10",  26l;
      "s11",  27l;
      "t3",   28l;
      "t4",   29l;
      "t5",   30l;
      "t6",   31l;
    ]

  let () =
    for i = 0 to 31 do
      Hashtbl.add regs ("x" ^ (Int.to_string i)) (Int32.of_int i)
    done

}

let digit   = ['0'-'9']
let integer = ('-')? digit+

let alpha = ['a'-'z']
let ident = alpha (alpha | digit)*
let label = '.' ident

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
  | ident as id
    {
      let open Hashtbl in
      let instr =
        if mem r_instructions id then
          (
            let r   = find r_inst id    in
            let rd  = parse_reg lexbuf  in
            let rs1 = parse_reg lexbuf  in
            let rs2 = parse_reg lexbuf  in
            R(r, rd, rs1, rs2)
          )
        else if mem i_instructions id then
          (
            let r   = find i_inst id   in
            let rd  = parse_reg lexbuf in
            let rs1 = parse_reg lexbuf in
            let imm = parse_imm lexbuf in
            I(r, rd, rs1, imm)
          )
        else if mem s_instructions id then
          (
            let r   = find s_inst id   in
            let rs2 = parse_reg lexbuf in
            let rs1 = parse_reg lexbuf in
            let imm = parse_imm lexbuf in
            S(r, rs2, rs1, imm)
          )
        else if mem b_instructions id then
          (
            let r   = find b_inst id   in
            let rs1 = parse_reg lexbuf in
            let rs2 = parse_reg lexbuf in
            let imm = parse_imm lexbuf in
            B(r, rs1, rs2, imm)
          )
        else if mem u_instructions id then
          (
            let r   = find u_inst id   in
            let rd  = parse_reg lexbuf in
            let imm = parse_imm lexbuf in
            U(r, rd, imm)
          )
        else if mem j_instructions id then
          (
            let r   = find j_inst id   in
            let rd  = parse_reg lexbuf in
            let imm = parse_imm lexbuf in
            J(r, rd, imm)
          )
        else (raise (Lexing_error id))
      in Instr(i, instr)
    }
  | _ as c
    {
      raise (Lexing_error (String.make 1 c))
    }

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
