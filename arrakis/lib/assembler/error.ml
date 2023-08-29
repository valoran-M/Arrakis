type lexing_error = Register | Imm | Inst

exception Lexing_error of int * lexing_error * string

type translate_error = Label_not_exists of string

exception Translate_error of int * translate_error
