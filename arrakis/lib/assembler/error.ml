type assembler_error =
  | Unknown_Label  of string
                   (* val     min     max     *)
  | Interval_imm   of int32 * int32 * int32
  | Parsing_error  of string
  | Lexing_error   of string

exception Assembler_error of int * assembler_error

let raise_unclosed line =
  raise (Assembler_error (line, Lexing_error "\" unclosed"))
