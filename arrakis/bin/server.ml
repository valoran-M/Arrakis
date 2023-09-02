let max_read = 1024
let buffer = Bytes.create 1024

let loop in_channel out_channel =
  let _ = In_channel.input in_channel buffer 0 1024 in
  Printf.printf "%s\n" (Bytes.to_string buffer);
  Out_channel.output out_channel (Bytes.of_string "coucou\n") 0 7

let start_server file =
  Unix.establish_server loop (Unix.ADDR_UNIX file)

