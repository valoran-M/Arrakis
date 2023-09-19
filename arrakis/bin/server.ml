open Unix

let max_read = 1024
let buffer = Bytes.create 1024

let rec loop arch history label addr_debug line_debug
             in_channel out_channel syscall =
  match In_channel.input_line in_channel with
  | Some s ->
    (try
      let input = String.trim s in
      let history = (match String.split_on_char ' ' input with
      | command :: args ->
        Shell.parse_command out_channel arch history command args label
          addr_debug line_debug syscall;
      | [] -> history) in
        loop arch history label addr_debug line_debug
             in_channel out_channel syscall
    with Shell.Shell_exit -> history)
  | None ->
    loop arch history label addr_debug line_debug in_channel out_channel syscall

let rec accept_non_intr s =
  try accept ~cloexec:true s
  with Unix_error (EINTR, _, _) -> accept_non_intr s

let start_server file arch history label addr_debug line_debug syscall =
  let sockaddr = ADDR_UNIX file in
  let sock = socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 5;
  let (s, _caller) = accept_non_intr sock in
  let inchan  = in_channel_of_descr s in
  let outchan = Format.formatter_of_out_channel (out_channel_of_descr s) in
  Syscall.Utils.set_stdout s s s;
  ignore (loop arch history label addr_debug line_debug inchan outchan syscall);
  close sock;
  unlink file

