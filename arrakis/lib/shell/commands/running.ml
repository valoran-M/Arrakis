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

(* Step ---------------------------------------------------------------------- *)

let step_execute _args (state : Types.state) =
  let open Syscall.Types     in
  let open Simulator.Execute in
  let program_end, history =
    if state.program_end
    then (
    Format.fprintf state.out_channel
        "\n%a Program has exited, can't run further.@." error ();
    true, state.history
  ) else
    match exec_instruction state.arch state.history with
    | Continue (_, history)  -> state.program_end, history
    | Zero        ->
      Format.fprintf state.out_channel
        "\n%a Exiting without an exit syscall.@." warning ();
      true, state.history
    | Sys_call history  ->
      match state.syscall state.out_channel state.arch with
      | Continue  -> state.program_end, history
      | Exit code ->
        Format.fprintf state.out_channel
          "\n%a Exiting with code @{<fg_yellow>'%d'@}.@."
          info ()
          code;
          true, history
    in
    {
      state with
      program_end;
      history;
    }

let step : Types.cmd = {
  long_form   = "step";
  short_form  = "s";
  name        = "(s)tep";
  description = "Execute next instruction.";
  execute     = step_execute;
}

(* Next --------------------------------------------------------------------- *)

let next_execute args (state : Types.state) =
  let rec sub first (state : Types.state) =
    let addr = Arch.Cpu.get_pc state.arch.cpu in
    if not state.program_end && (first || not (Hashtbl.mem state.breakpoints addr)) then
      let new_state = step_execute args state in
      sub false new_state
    else state
  in
  sub true state

let next : Types.cmd = {
  long_form   = "next";
  short_form  = "n";
  name        = "(n)ext";
  description = "Run code until the next breakpoint.";
  execute     = next_execute;
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
  description = "Run code until the end.";
  execute     = run_execute;
}

(* Prev --------------------------------------------------------------------- *)

let prev_execute _args (state : Types.state) =
  let history =
  try Simulator.History.step_back state.arch state.history
  with Simulator.History.History_Empty ->
    Format.fprintf state.out_channel "\n%a History is empty.@." error ();
    state.history
  in
  { state with history }

let prev : Types.cmd = {
  long_form   = "previous";
  short_form  = "pre";
  name        = "(pre)vious";
  description = "Revert previous step.";
  execute     = prev_execute;
}

(* Reset -------------------------------------------------------------------- *)

let reset_execute _args (state : Types.state) =
  {
    state with
    program_end = false;
    history     = Simulator.History.reset state.arch state.history;
  }

let reset : Types.cmd = {
  long_form   = "reset";
  short_form  = "res";
  name        = "(res)et";
  description = "Recovery of the simulator's initial state.";
  execute     = reset_execute;
}
