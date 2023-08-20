open Simulator
open Lexer

let translate code =
  let _mem = Memory.make () in
  Lexer.prog 0 (Lexing.from_string code)
