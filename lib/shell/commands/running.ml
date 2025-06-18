(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Error
open Common.Pp

(*
   This file define the following commands:
  - step
  - run
  - continue
  - prev
  - reset
*)

(* Utils -------------------------------------------------------------------- *)

let program_not_run (state : Types.state) =
    Format.fprintf state.out_channel "%a Program is not running.@." info ()

let execute_syscal (state : Types.state) =
  (* reset shell *)
  ignore (state.exit ());
  let r = state.ecall state.out_channel state.arch in
  ignore (state.init ());
  r

let exec_ecall (state : Types.state) history =
  match execute_syscal state with
  | Continue  -> state.program_run, history
  | Exit code ->
    Format.fprintf state.out_channel
      "%a Exiting with code @{<fg_yellow>'%d'@}@." info () code;
    false, history

(* Take one step forward *)
let one_step (state : Types.state) =
  if not state.program_run then assert false;
  let program_run, history =
    match Simulator.Execute.exec_instruction state.arch state.history with
    | Continue (_, history) -> state.program_run, history
    | Sys_call history      -> exec_ecall state history
    | Zero ->
      Format.fprintf state.out_channel
        "%a Exiting without an exit syscall@." warning ();
      false, state.history
  in
  state.program_run <- program_run;
  state.history     <- history

(* Take n step forward *)
let rec n_step n state =
  one_step state;
  if n > 1 && state.program_run
  then n_step (n-1) state

(* Take one step backward *)
let one_bstep (state : Types.state) =
  try state.history <- History.step_back state.arch state.history;
      state.program_run <- true
  with History.History_Empty ->
    Format.fprintf state.out_channel "%a History is empty.@." error ()

(* Take n step backward *)
let rec n_bstep n state =
  one_bstep state;
  if n > 1 && state.program_run
  then n_bstep (n-1) state

let is_call code =
  if Int32.logand code 0b1111111l <> 0b1100111l
  then false
  else
    let decode = Instructions.I.decode code in
    (* JALR *)
    decode.fc3 = 0x0 &&
    (decode.rs1 = 1 && decode.rdt = 1)

let is_ret code =
  if Int32.logand code 0b1111111l <> 0b1100111l
  then false
  else
    let decode = Instructions.I.decode code in
    (* RET *)
    decode.fc3 = 0x0 && decode.rs1 = 1 && decode.rdt = 0

let pass_function depth (state : Types.state) =
  let arch = state.arch in
  let rec loop depth =
    let code = Arch.Memory.get_int32 arch.memory (Arch.Cpu.get_pc arch.cpu) in
    one_step state;
    if state.program_run then
      match is_call code, is_ret code with
      | true,  false -> loop (depth + 1)
      | false, true  -> if depth > 1 then loop (depth - 1)
      | false, false -> if depth > 0 then loop depth
      | _ -> assert false
  in
  loop depth

(* Step ---------------------------------------------------------------------- *)

let step_execute args (state : Types.state) =
  if not state.program_run
  then program_not_run state
  else
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
    sub        = [];
    args       = Nil; }

(* Continue ----------------------------------------------------------------- *)

let rec next_breakpoint (first : bool) (state : Types.state) =
  let addr = Arch.Cpu.get_pc state.arch.cpu in
  if state.program_run && (first || not (Hashtbl.mem state.breakpoints addr))
  then (one_step state; next_breakpoint false state)

let rec n_next_breakpoint n state =
  next_breakpoint true state;
  if n > 1
  then n_next_breakpoint (n-1) state

let continue_execute args (state : Types.state) =
  if not state.program_run
  then program_not_run state
  else
    match args with
    | [] -> next_breakpoint true state
    | [count]  -> (
      try n_next_breakpoint (int_of_string count) state
      with Failure _ -> raise (Shell_error Bad_Usage))
    | _  -> raise (Shell_error Bad_Usage)

let continue : Types.cmd =
  { long_form   = "continue";
    short_form  = "c";
    name        = "(c)ontinue";
    short_desc  = "Run code until the next breakpoint";
    long_desc   = ["Usage: continue <count>"];
    execute     = continue_execute;
    sub         = [];
    args        = Nil; }

(* Next --------------------------------------------------------------------- *)

let run_next _ (state : Types.state) =
  if not state.program_run
  then program_not_run state
  else pass_function 0 state

let next : Types.cmd =
  { long_form   = "next";
    short_form  = "n";
    name        = "(n)ext";
    short_desc  = "Like step, but does not step into functions";
    long_desc   = ["Usage: next <count>"];
    execute     = run_next;
    sub         = [];
    args        = Nil; }

(* Finish ------------------------------------------------------------------- *)

let finish_execute _ (state : Types.state) =
  if not state.program_run
  then program_not_run state
  else pass_function 1 state

let finish : Types.cmd =
  { long_form   = "finish";
    short_form  = "f";
    name        = "(f)inish";
    short_desc  = "Continue until the current function is finished";
    long_desc   = [
      "Run the program until a ret instruction is reached or pc is out of bound"
    ];
    execute     = finish_execute;
    sub         = [];
    args        = Nil; }

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
    sub         = [];
    args        = Nil; }

(* Run ---------------------------------------------------------------------- *)

let run_execute args (state : Types.state) =
  if not state.program_run then (
    Simulator.Arguments.write_arguments state.arch args;
    state.program_run <- true;
    state.history     <- History.reset state.arch state.history);
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
    sub         = [];
    args        = Nil; }

