let usage = "usage: arrakis <file>"

let show_version = ref false

let input_file       = ref []
let set_input_file f = input_file := f :: !input_file

let unix_socket = ref false
let unix_file   = ref (Unix.getcwd () ^ "/socket")

let no_color   = ref false
let allow_root = ref false
let no_shell   = ref false

let env = ref "unix"

let spec = [
  ("-U",            Arg.Set unix_socket,       " Use unix socket"                  );
  ("-f",            Arg.Set_string unix_file , " Unix socket's file"               );
  ("-e",            Arg.Set_string env,        "<venus|unix> Set env for ecalls."  );
  ("--no-color",    Arg.Set no_color,          " Don't use color in output"        );
  ("--allow-root",  Arg.Set allow_root,        " Allow usage in root mode."        );
  ("-ns",           Arg.Set no_shell,          " Run the program and exit."        );
  ("--no-shell",    Arg.Set no_shell,          " Run the program and exit."        );
  ("-v",            Arg.Set show_version,      " Show version number and exit."    );
  ("--version",     Arg.Set show_version,      " Show version number and exit."    );
]

let spec = Arg.align spec

let () =
  Arg.parse spec set_input_file usage

let show_version = !show_version

let input_file  = !input_file

let unix_socket = !unix_socket
let unix_file   = !unix_file

let no_color    = !no_color
let allow_root  = !allow_root
let no_shell    = !no_shell

let env = !env
