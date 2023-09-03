let usage = "usage: arrakis <file>"

let input_file  = ref ""
let set_input_file f =
  if not (Sys.file_exists f) then raise (Arg.Bad "Input file does not exists.")
  else input_file := f

let unix_socket = ref false
let unix_file   = ref "./socket"

let no_color   = ref false

let spec = [
  ("-U",         Arg.Set unix_socket,       "Use unix socket");
  ("--no-color", Arg.Set no_color,          "Don't use color in error output");
  ("-f",         Arg.Set_string unix_file , "Unix socket's file");
]

let alspec = Arg.align spec

let () =
  Arg.parse alspec set_input_file usage

let unix_socket = !unix_socket
let unix_file   = !unix_file

let input_file = !input_file
let no_color   = !no_color
