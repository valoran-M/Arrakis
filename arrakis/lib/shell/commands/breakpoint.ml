(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Format
open Global_utils.Print

exception End_loop

let line_breakpoint (state : Types.state) arg =
  match int_of_string_opt arg with
  | None      ->
      fprintf state.out_channel "%a '%s' is not a number.@." error () arg
  | Some line ->
    try
      let number = Hashtbl.length state.breakpoints in
      let addr   = Assembler.Debug.get_addr state.debug line in
      Hashtbl.add state.breakpoints addr number;
      fprintf state.out_channel
        "%a Created breakpoint %d at 0x%x@."
        info () number (Int32.to_int addr)
    with Not_found ->
      fprintf state.out_channel
        "%a Line %d does not contain code.@." error () line

let addr_breakpoint (state : Types.state) arg =
  let number = Hashtbl.length state.breakpoints in
  match Int32.of_string_opt arg with
  | Some addr ->
    Hashtbl.add state.breakpoints addr number;
    fprintf state.out_channel
      "%a Created breakpoint %d at 0x%x.@."
      info () number (Int32.to_int addr)
  | None ->
    match Assembler.Label.get_address_opt state.labels arg with
    | None ->
      fprintf state.out_channel
        "%a Function '%s' not defined.@." error () arg
    | Some addr ->
      Hashtbl.add state.breakpoints addr number;
      fprintf state.out_channel
        "%a Created breakpoint %d at 0x%x.@."
        info () number (Int32.to_int addr)

let remove_breakpoint (state : Types.state) arg =
  try
    let breakpoint = int_of_string arg in
    Hashtbl.iter (fun addr line ->
      if line = breakpoint then (
        Hashtbl.remove state.breakpoints addr;
        fprintf state.out_channel
          "%a Breakpoint '%d' was removed.@." info () breakpoint;
        raise End_loop
    )) state.breakpoints
  with
    | End_loop -> ()
    | _        ->
      fprintf state.out_channel "%a Breakpoint '%s' does not exist.@."
        error () arg

let iter f l (state : Types.state) =
  if List.length l == 0 then
      fprintf state.out_channel "%a Command require at least one argument.@." error ()
  else List.iter f l

let execute args (state : Types.state) =
  begin match args with
  | "line"   :: args
  | "l"      :: args -> iter (line_breakpoint state) args state
  | "addr"   :: args
  | "a"      :: args -> iter (addr_breakpoint state) args state
  | "remove" :: args
  | "r"      :: args -> iter (remove_breakpoint state) args state
  | "print"  :: _
  | "p"      :: _    ->
      Hashtbl.iter (fun addr number ->
        fprintf state.out_channel "%3d -> 0x%08x@."
        number (Global_utils.Integer.int32_to_int addr)) state.breakpoints
  | _ -> Help.breakpoint state.out_channel
  end;
  state (* As we changed it's hashtbl, the state is actually changed here *)

let breakpoint : Types.cmd = {
  long_form   = "breakpoint";
  short_form  = "b";
  name        = "(b)reakpoint";
  description = "Create breakpoints";
  execute;
}
