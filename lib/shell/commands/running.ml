(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Error
open Common.Print

(*
   This file define the following commands:
  - step
  - run
  - continue
  - prev
  - reset
*)

(* Utils -------------------------------------------------------------------- *)

(* Take one step forward *)
let one_step (state : Types.state) =
  let open Syscall.Types     in
  let open Simulator.Execute in
  let program_run, history =
    if not state.program_run then (
      Format.fprintf state.out_channel "%a Program is not running.@." error ();
      false, state.history
    ) else match exec_instruction state.arch state.history with
    | Continue (_, history)  -> state.program_run, history
    | Zero ->
      Format.fprintf state.out_channel
        "%a Exiting without an exit syscall@." warning ();
      false, state.history
    | Sys_call history  ->
      match state.syscall state.out_channel state.arch with
      | Continue  -> state.program_run, history
      | Exit code ->
        Format.fprintf state.out_channel
          "%a Exiting with code @{<fg_yellow>'%d'@}@." info () code;
        false, history
  in
  { state with
    program_run;
    history; }

(* Take n step forward *)
let rec n_step n state =
  if n > 0
  then n_step (n-1) (one_step state)
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
  if n > 0
  then n_bstep (n-1) (one_bstep state)
  else state

let is_call code =
  if Int32.logand code 0b1111111l <> 0b1100111l
  then false
  else
    let decode = Instructions.I.decode code in
    (* JALR *)
    decode.fc3 = 0x0 &&
    ((decode.rs1 = 1 && decode.rdt = 1) ||
     (decode.rs1 = 6 && decode.rdt = 0))

let is_ret code =
  if Int32.logand code 0b1111111l <> 0b1100111l
  then false
  else
    let decode = Instructions.I.decode code in
    (* RET *)
    decode.fc3 = 0x0 && decode.rs1 = 1 && decode.rdt = 0

let rec pass_function depth (state : Types.state) =
  if not state.program_run then state else
  let arch = state.arch in
  let code = Arch.Memory.get_int32 arch.memory (Arch.Cpu.get_pc arch.cpu) in
  if is_call code
  then pass_function (depth + 1) (one_step state)
  else if is_ret code then
    if depth = 1
    then one_step state
    else pass_function (depth - 1) (one_step state)
  else
    if depth = 0
    then one_step state
    else pass_function depth (one_step state)

(* Step ---------------------------------------------------------------------- *)

let step_execute args (state : Types.state) =
  match args with
  | []         -> one_step state
  | count :: _ ->
    try n_step (int_of_string count) state
    with _ -> raise (Shell_error (Bad_Usage))

let step : Types.cmd =
  { long_form  = "step";
    short_form = "s";
    name       = "(s)tep";
    short_desc = "Execute next instruction";
    long_desc  = ["Usage: step <count>"];
    execute    = step_execute;
    sub        = []; }

(* Continue ----------------------------------------------------------------- *)

let rec next_breakpoint (first : bool) (state : Types.state) =
  let addr = Arch.Cpu.get_pc state.arch.cpu in
  if state.program_run && (first || not (Hashtbl.mem state.breakpoints addr))
  then next_breakpoint false (one_step state)
  else state

let rec n_next_breakpoint n state =
  if n > 0
  then n_next_breakpoint (n-1) (next_breakpoint true state)
  else state

let continue_execute args (state : Types.state) =
  match args with
  | [count]  -> (
    try n_next_breakpoint (int_of_string count) state
    with _ -> raise (Shell_error Bad_Usage))
  | [] -> next_breakpoint true state
  | _ -> raise (Shell_error Bad_Usage)

let continue : Types.cmd =
  { long_form   = "continue";
    short_form  = "c";
    name        = "(c)ontinue";
    short_desc  = "Run code until the next breakpoint";
    long_desc   = ["Usage: continue <count>"];
    execute     = continue_execute;
    sub         = []; }

(* Next --------------------------------------------------------------------- *)

let run_next _ (state : Types.state) =
  pass_function 0 state

let next : Types.cmd =
  { long_form   = "next";
    short_form  = "n";
    name        = "(n)ext";
    short_desc  = "Like step, but does not step into functions";
    long_desc   = ["Usage: next <count>"];
    execute     = run_next;
    sub         = [] }

(* Finish ------------------------------------------------------------------- *)

let finish_execute _ (state : Types.state) =
  pass_function 1 state

let finish : Types.cmd =
  { long_form   = "finish";
    short_form  = "f";
    name        = "(f)inish";
    short_desc  = "Continue until the current function is finished";
    long_desc   = [
      "Run the program until a ret instruction is reached or pc is out of bound"
    ];
    execute     = finish_execute;
    sub         = [] }

(* Previous ----------------------------------------------------------------- *)

let pre_execute args (state : Types.state) =
  match args with
  | []         -> one_bstep state
  | count :: _ ->
    try n_bstep (int_of_string count) state
    with _ -> raise (Shell_error (Bad_Usage))

let previous : Types.cmd =
  { long_form   = "previous";
    short_form  = "p";
    name        = "(p)revious";
    short_desc  = "Revert previous step";
    long_desc   = ["Usage: previous <count>"];
    execute     = pre_execute;
    sub         = []; }

(* Run ---------------------------------------------------------------------- *)

let run_execute args (state : Types.state) =
  let state =
    { state with
      program_run = true;
      history     = History.reset state.arch state.history; }
  in
  Simulator.Arguments.write_arguments state.arch args;
  next_breakpoint false state

let run : Types.cmd =
  { long_form   = "run";
    short_form  = "r";
    name        = "(r)un";
    short_desc  = "Start the execution";
    long_desc   = [
      "Usage: run <arg1> ... <argn>";
      "Start executing the program and give <arg1> ... <argn> as arguments.";
    ];
    execute     = run_execute;
    sub         = []; }

