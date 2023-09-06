open Unix

let max_read = 1024
let buffer = Bytes.create 1024

let rec loop arch label addr_debug line_debug in_channel out_channel =
  match In_channel.input_line in_channel with
  | Some s ->
    (try
      let input = String.trim s in
      (match String.split_on_char ' ' input with
      | command :: args ->
          Shell.parse_command out_channel arch command args label
            addr_debug line_debug;
      | [] -> ());
      loop arch label addr_debug line_debug in_channel out_channel
    with Shell.Shell_exit -> ())
  | None -> 
    loop arch label addr_debug line_debug in_channel out_channel

let rec accept_non_intr s =
  try accept ~cloexec:true s
  with Unix_error (EINTR, _, _) -> accept_non_intr s

let start_server file arch label addr_debug line_debug =
  let sockaddr = ADDR_UNIX file in
  let sock = socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 5;
  let (s, _caller) = accept_non_intr sock in
  let inchan = in_channel_of_descr s in
  let outchan = out_channel_of_descr s in
  loop arch label addr_debug line_debug inchan outchan;
  close sock;
  unlink file

