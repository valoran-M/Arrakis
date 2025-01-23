(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type cmd = {

  (* Name to type to invoke said command *)
  long_form  : string;

  (* Short way to invoke command, should try to be one character max *)
  short_form : string;

  (* Shown in the help menu *)
  name       : string;
  short_desc : string;
  long_desc  : string list;

  (* Do the actual action of the command *)
  (* Please note that as the state contain mutable values, the execute function
    may alter the state given as argument *)
  execute     : string list -> state -> state;

  sub         : cmd list;
}

and state = {

  (* Shell state *)
  init         : string -> bool;
  exit         : unit -> bool;
  input        : unit -> Io.Std.ret;
  out_channel  : Format.formatter;
  cmds         : cmd list;
  cmds_history : string array;
  breakpoints  : (int32, int) Hashtbl.t;
  program_run  : bool;

  (* Program state *)
  history      : History.t;
  arch         : Arch.Riscv.t;
  debug        : Assembler.Debug.t;
  labels       : Assembler.Label.t;
  ecall        : Ecall.Types.ecall;

}

