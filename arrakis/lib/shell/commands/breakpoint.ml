(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Format
open Global_utils.Print

exception End_loop

let iter f args (state : Types.state) =
  (if List.length args == 0 then
      fprintf state.out_channel "%a Command require at least one argument@." error ()
  else
    List.iter (f state) args);
  state

(* Line --------------------------------------------------------------------- *)

let execute_line (state : Types.state) arg =
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

let breakpoint_line : Types.cmd =
  { long_form   = "line";
    short_form  = "l";
    name        = "(l)ine";
    short_desc  = "Add breakpoints on specified lines";
    long_desc   = "";
    execute     = iter execute_line;
    sub         = []; }

(* Addr --------------------------------------------------------------------- *)

let execute_addr (state : Types.state) arg =
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

let breakpoint_addr : Types.cmd =
  { long_form   = "addr";
    short_form  = "a";
    name        = "(a)ddr";
    short_desc  = "Add breakpoints on specified address";
    long_desc   = "";
    execute     = iter execute_addr;
    sub         = []; }

(* Print -------------------------------------------------------------------- *)

let execute_print _args (state : Types.state) =
  Hashtbl.iter (fun addr number ->
    let open Global_utils.Integer in
    fprintf state.out_channel "%3d -> 0x%08x@." number (int32_to_int addr))
  state.breakpoints

let breakpoint_print : Types.cmd =
  { long_form   = "print";
    short_form  = "p";
    name        = "(p)rint";
    short_desc  = "Print all breakpoints";
    long_desc   = "";
    execute     = iter execute_addr;
    sub         = []; }

(* Remove ------------------------------------------------------------------- *)

let execute_remove (state : Types.state) arg =
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

let breakpoint_remove : Types.cmd =
  { long_form   = "remove";
    short_form  = "r";
    name        = "(r)emove";
    short_desc  = "Remove specified breakpoints";
    long_desc   = "";
    execute     = iter execute_remove;
    sub         = []; }

let rec breakpoint : Types.cmd =
  { long_form   = "breakpoint";
    short_form  = "b";
    name        = "(b)reakpoint";
    short_desc  = "Create breakpoints";
    long_desc   = "";
    execute     = (fun _ state -> Help.command breakpoint state);
    sub         = [
      breakpoint_addr;
      breakpoint_line;
      breakpoint_print;
      breakpoint_remove;
    ]; }
