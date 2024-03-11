(* Read a line from stdin:
  - Auto complete when '\t' is read
  - Go back in history with arrows
  - Allow erasing the current line with backspace
  - Coloring when a command is valid (green) or invalid (red)
*)
let line (_state : Types.state)  : string =
  (* TODO *)
  read_line ()

