exception Shell_Exit

let quit : Types.cmd = 
  {
    long_form   = "quit";
    short_form  = "q";
    name        = "(q)uit";
    short_desc  = "Exit shell";
    long_desc   = "";
    execute     = (fun _ _ -> raise Shell_Exit);
    sub         = [];
  }
