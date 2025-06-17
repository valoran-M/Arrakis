(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Types
open Error

exception Shell_invalid_arg of string list

let cmd_eq (command : string) (cmd : Types.cmd) =
  cmd.long_form = command || cmd.short_form = command

let arg_to_int32 (state : Types.state) arg =
  if arg = "pc" then Arch.Cpu.get_pc state.arch.cpu
  else match Assembler.Regs.of_string_opt arg with
  | Some s -> Arch.Cpu.get_reg state.arch.cpu (Int32.to_int s)
  | None ->
      match Assembler.Label.get_address_opt state.labels arg with
      | Some s -> s
      | None   -> Int32.of_string arg

let get_size labels label default =
  match Assembler.Label.get_size_opt labels label with
  | Some s -> s
  | None   -> default

let rec args_default args default =
  match args, default with
  | [], d -> d 
  | a :: la, _ :: ld -> a :: args_default la ld
  | _,  _ -> assert false

let all_commands_sl (all_commands : cmd list) =
  List.fold_left (fun a c -> c.long_form :: a) [] all_commands

let all_commands_string (all_commands : cmd list) =
  List.fold_left (fun a c -> c.long_form ^ " " ^ a) "" all_commands

let find_cmd cmd lc =
  try  List.find (cmd_eq cmd) lc
  with Not_found -> raise (Shell_error Cmd_Not_Found)

let find_cmd_opt cmd lc = List.find_opt (cmd_eq cmd) lc

let rec list_rm_last l =
  match l with
  | []     -> raise Not_found
  | [c]    -> [], c
  | c :: l -> let nl, last = list_rm_last l in c :: nl, last

let rec get_sub_cmd (cmd : cmd) all_cmd args =
  match args with
  | []     -> cmd, all_cmd, []
  | s :: a ->
    match find_cmd_opt s cmd.sub with
    | None   -> cmd, all_cmd, args
    | Some c -> get_sub_cmd c (c :: all_cmd) a

let get_cmd_l line (state : Types.state) : cmd * cmd list * string list =
  match line with
  | []     -> raise (Shell_error Cmd_Not_Found)
  | c :: a ->
    let cmd = find_cmd c state.cmds in
    get_sub_cmd cmd [cmd] a

let get_cmd (line : string) (state : state) : cmd * cmd list * string list =
  match String.split_on_char ' ' line with
  | []     -> raise (Shell_error Cmd_Not_Found)
  | c :: a ->
    let cmd = find_cmd c state.cmds in
    get_sub_cmd cmd [cmd] a


let print_cmds cmds s =
  Format.fprintf s.out_channel "\n%!";
  List.iter (fun c -> Format.fprintf s.out_channel "%-20s" c) cmds;
  Format.fprintf s.out_channel "\n%!"

