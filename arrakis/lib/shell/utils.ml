
let cmd_eq (command : string) (cmd : Types.cmd) =
  cmd.long_form = command || cmd.short_form = command
