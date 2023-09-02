type server = (Unix.file_descr * in_channel * out_channel)

let max_read = 1024
let buffer = Bytes.create 1024

let loop ((_, in_channel, out_channel): server) =
  let _ = In_channel.input in_channel buffer 0 1024 in
  Printf.printf "%s\n" (Bytes.to_string buffer);
  Out_channel.output out_channel (Bytes.of_string "coucou\n") 0 8

let start_server file =
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_UNIX file);
  let ci, co = Unix.open_connection (Unix.ADDR_UNIX file) in
  (socket, ci, co)

let close_server ((socket, in_channel, _): server) =
  Unix.shutdown_connection in_channel;
  Unix.shutdown socket SHUTDOWN_ALL

