let max_read = 1024
let buffer = Bytes.create 1024

let loop arch label debug in_channel _out_channel =
  let i = In_channel.input in_channel buffer 0 1024 in
  try
    let input = String.trim (String.sub (Bytes.to_string buffer) 0 i) in
    match String.split_on_char ' ' input with
    | command :: args ->
        Shell.parse_command arch command args label debug;
    | [] -> ()
  with Shell.Shell_exit -> Printf.printf "exit\n"; exit 0

let start_server file arch label debug =
  Unix.establish_server (loop arch label debug) (Unix.ADDR_UNIX file);
  exit 0

