let usage = "usage: arrakis <file>"

let input_file  = ref ""
let set_input_file f =
  if not (Sys.file_exists f) then raise (Arg.Bad "Input file does not exists")
  else input_file := f
let unix_socket = ref false
let unix_file   = ref (Unix.getcwd () ^ "/socket")

let no_color   = ref false
let allow_root = ref false
let no_shell   = ref false

let env = ref "unix"

let spec = [
  ("-U",            Arg.Set unix_socket,       "Use unix socket");
  ("-f",            Arg.Set_string unix_file , "Unix socket's file");
  ("-e",            Arg.Set_string env, "Set env for ecalls. <venus | unix>");
  ("--no-color",    Arg.Set no_color,          "Don't use color in output");
  ("--allow-root",  Arg.Set allow_root,        "Allow usage in root mode.");
  ("-ns",           Arg.Set no_shell,          "Run the program and exit.");
  ("--no-shell",    Arg.Set no_shell,          "Run the program and exit.");
]

let alspec = Arg.align spec

let () =
  Arg.parse alspec set_input_file usage

let input_file  = !input_file

let unix_socket = !unix_socket
let unix_file   = !unix_file

let no_color    = !no_color
let allow_root  = !allow_root
let no_shell    = !no_shell

let env = !env
