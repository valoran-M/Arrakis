open Simulator

let translate code =
  let _mem = Memory.make () in
  let lines = String.split_on_char '\n' code in
  List.iter (fun _line ->
    ()
  ) lines
