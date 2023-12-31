(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

let usage = "usage: arrakis <file>"

let show_version = ref false

let input_file       = ref []
let set_input_file f = input_file := f :: !input_file

let unix_socket = ref false
let unix_file   = ref (Unix.getcwd () ^ "/socket")

let no_color   = ref false
let allow_root = ref false
let just_run   = ref false

let env = ref "unix"

let spec = [
  ("-U",            Arg.Set unix_socket,       " Use unix socket"                 );
  ("-f",            Arg.Set_string unix_file , " Unix socket's file"              );
  ("-e",            Arg.Set_string env,        "<venus|unix> Set env for ecalls"  );
  ("--no-color",    Arg.Set no_color,          " Don't use color in output"       );
  ("--allow-root",  Arg.Set allow_root,        " Allow usage in root mode"        );
  ("-r",            Arg.Set just_run,          " Run the program and exit"        );
  ("--just-run",    Arg.Set just_run,          " Run the program and exit"        );
  ("--version",     Arg.Set show_version,      " Show version number and exit"    );
]

let () =
  Arg.parse (Arg.align spec) set_input_file usage

let show_version = !show_version

let input_file  = !input_file

let unix_socket = !unix_socket
let unix_file   = !unix_file

let no_color    = !no_color
let allow_root  = !allow_root
let just_run    = !just_run

let env = !env
