(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type args =
  | Nil
  | Regs
  | Text_addr of args
  | Text_size of args
  | Data_addr of args
  | Data_size of args

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
  execute     : string list -> state -> unit;

  sub         : cmd list;
  args        : args;
}

and state = {

  (* Shell state *)
  init         : unit -> bool;
  exit         : unit -> bool;
  input        : Io.Std.t -> Io.Std.ret;
  out_channel  : Format.formatter;
  cmds         : cmd list;
  breakpoints  : (int32, int) Hashtbl.t;


  (* Program state *)
  mutable program_run : bool;
  mutable history     : History.t;

  arch         : Arch.Riscv.t;
  debug        : Assembler.Debug.t;
  labels       : Assembler.Label.t;
  ecall        : Ecall.Types.ecall;

}

