(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

let usage = "usage: arrakis <file>"

let show_version = ref false

let input_file       = ref []
let set_input_file f = input_file := f :: !input_file

let no_color   = ref false
let allow_root = ref false
let run        = ref None

let set_run args =
  run := Some args

let env = ref "unix"

let spec = [
  ("-e",            Arg.Set_string env,        "<venus|unix> Set env for ecalls"  );
  ("--no-color",    Arg.Set no_color,          " Don't use color in output"       );
  ("--allow-root",  Arg.Set allow_root,        " Allow usage in root mode"        );
  ("--run",         Arg.Rest_all set_run,      " Run the program and exit"        );
  ("--version",     Arg.Set show_version,      " Show version number and exit"    );
]

let () =
  Arg.parse (Arg.align spec) set_input_file usage

let show_version = !show_version

let input_file  = !input_file

let no_color    = !no_color
let allow_root  = !allow_root
let run         = !run

let env = !env
