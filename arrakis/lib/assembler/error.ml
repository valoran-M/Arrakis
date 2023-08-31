type lexing_error = Register | Imm | Inst

exception Lexing_error of int * lexing_error * string

type assembler_error =
  | Label_not_exists of string
  | Interval_imm of int32 * int32 * int32

exception Assembler_error of int * assembler_error
