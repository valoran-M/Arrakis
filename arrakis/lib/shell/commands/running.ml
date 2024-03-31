(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Global_utils.Print

(*
   This file define the following commands:
  - step
  - run
  - next
  - prev
  - reset
*)

(* Utils -------------------------------------------------------------------- *)

(* Take one step forward *)
let one_step (state : Types.state) =
  let open Syscall.Types     in
  let open Simulator.Execute in
  let program_end, history =
    if state.program_end then (
      Format.fprintf state.out_channel
        "%a Program has exited, can't run further.@." error ();
      true, state.history
    ) else
    match exec_instruction state.arch state.history with
    | Continue (_, history)  -> state.program_end, history
    | Zero ->
      Format.fprintf state.out_channel
        "%a Exiting without an exit syscall@." warning ();
      true, state.history
    | Sys_call history  ->
      match state.syscall state.out_channel state.arch with
      | Continue  -> state.program_end, history
      | Exit code ->
        Format.fprintf state.out_channel
          "%a Exiting with code @{<fg_yellow>'%d'@}@." info () code;
        true, history
  in
  {
    state with
    program_end;
    history;
  }

(* Take n step forward *)
let rec n_step n state =
  if n > 0 then n_step (n-1) (one_step state)
           else state

(* Take one step backward *)

let one_bstep (state : Types.state) =
  let history =
  try History.step_back state.arch state.history
  with History.History_Empty ->
    Format.fprintf state.out_channel "%a History is empty.@." error ();
    state.history
  in
  { state with history }

(* Take n step backward *)
let rec n_bstep n state =
  if n > 0 then n_bstep (n-1) (one_bstep state)
           else state

(* Step ---------------------------------------------------------------------- *)

let step_execute args (state : Types.state) =
  match args with
  | []         -> one_step state
  | count :: _ ->
      try
        n_step (int_of_string count) state
      with _ ->
        Format.printf "%a Incorrect argument @{<fg_yellow>'%s'@}@." error () count;
        state

let step : Types.cmd = {
  long_form  = "step";
  short_form = "s";
  name       = "(s)tep";
  short_desc = "Execute next instruction";
  long_desc  = "";
  execute    = step_execute;
  sub        = [];
}

(* Next --------------------------------------------------------------------- *)

let next_execute _args (state : Types.state) =
  let rec sub first (state : Types.state) =
    let addr = Arch.Cpu.get_pc state.arch.cpu in
    if not state.program_end && (first || not (Hashtbl.mem state.breakpoints addr)) then
      sub false (one_step state)
    else state
  in
  sub true state

let next : Types.cmd = {
  long_form   = "next";
  short_form  = "n";
  name        = "(n)ext";
  short_desc  = "Run code until the next breakpoint";
  long_desc   = "";
  execute     = next_execute;
  sub         = []
}

(* Run ---------------------------------------------------------------------- *)

let rec run_execute args (state : Types.state) =
  if not state.program_end then
    let new_state = step_execute args state in
    run_execute args new_state
  else state

let run : Types.cmd = {
  long_form   = "run";
  short_form  = "r";
  name        = "(r)un";
  short_desc  = "Run code until the end";
  long_desc   = "";
  execute     = run_execute;
  sub         = []
}

(* Prev --------------------------------------------------------------------- *)

let prev_execute args (state : Types.state) =
  match args with
  | []         -> one_bstep state
  | count :: _ ->
      try
        n_bstep (int_of_string count) state
      with _ ->
        Format.printf "%a Incorrect argument @{<fg_yellow>'%s'@}@." error () count;
        state

let prev : Types.cmd = {
  long_form   = "previous";
  short_form  = "pre";
  name        = "(pre)vious";
  short_desc  = "Revert previous step";
  long_desc   = "";
  execute     = prev_execute;
  sub         = []
}

(* Reset -------------------------------------------------------------------- *)

let reset_execute _args (state : Types.state) =
  {
    state with
    program_end = false;
    history     = History.reset state.arch state.history;
  }

let reset : Types.cmd = {
  long_form   = "reset";
  short_form  = "res";
  name        = "(res)et";
  short_desc  = "Recovery of the simulator's initial state";
  long_desc   = "";
  execute     = reset_execute;
  sub         = []
}
