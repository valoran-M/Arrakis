(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

let usage = "usage: arrakis <file>"

type t = {
  show_version  : bool;
  input_file    : string list;
  no_color      : bool;
  allow_root    : bool;
  env           : string;
  run           : string list option;
}

let get (): t =
  let show_version  = ref false   in
  let input_file    = ref []      in
  let no_color      = ref false   in
  let allow_root    = ref false   in
  let env           = ref "unix"  in
  let run           = ref None    in

  let set_input_file f = input_file := f :: !input_file in
  let set_run args = run := Some args in

  let spec = [
    ("-e",            Arg.Set_string env,   "<venus|unix> Set env for ecalls" );
    ("--no-color",    Arg.Set no_color,     " Don't use color in output"      );
    ("--allow-root",  Arg.Set allow_root,   " Allow usage in root mode"       );
    ("--run",         Arg.Rest_all set_run, " Run the program and exit"       );
    ("--version",     Arg.Set show_version, " Show version number and exit"   );
  ]
  in
  Arg.parse (Arg.align spec) set_input_file usage;
  {
    show_version  = !show_version;
    input_file    = !input_file;
    no_color      = !no_color;
    allow_root    = !allow_root;
    env           = !env;
    run           = !run;
  }
