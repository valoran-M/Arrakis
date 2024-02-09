exception Shell_Exit

let quit : Types.cmd = {
  long_form   = "quit";
  short_form  = "q";
  name        = "(q)uit";
  description = "Exit shell.";
  execute     = fun _ _ -> raise Shell_Exit;
}
