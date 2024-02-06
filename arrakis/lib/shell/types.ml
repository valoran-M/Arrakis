(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

type command = {

  (* Name to type to invoke said command *)
  long_form   : string;

  (* Short way to invoke command, should try to be one character max *)
  short_form  : string;

  (* Shown in the help menu *)
  name        : string;
  description : string;

  (* Do the actual action of the command *)
  (* Please note that as the state contain mutable values, the execute function
    may alter the state given as argument *)
  execute     : string list -> state -> state

}

and state = {

    out_channel : Format.formatter;

    history     : Simulator.History.t;
    arch        : Simulator.Arch.t;
    debug       : Assembler.Debug.t;
    labels      : Assembler.Label.t;

    breakpoints : (int32, int) Hashtbl.t;
    program_run : bool;
    program_end : bool;
    syscall     : Syscall.Types.syscall;

    commands    : (string, command) Hashtbl.t;

  }
