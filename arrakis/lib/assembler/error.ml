exception Lexing_error of int * string

type assembler_error =
  | Unknown_Label  of string
  | Interval_imm   of int32 * int32 * int32
  | Parsing_error  of string

exception Assembler_error of int * assembler_error
