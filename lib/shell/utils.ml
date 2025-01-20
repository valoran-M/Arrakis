(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

exception Shell_invalid_arg of string list

let cmd_eq (command : string) (cmd : Types.cmd) =
  cmd.long_form = command || cmd.short_form = command

let arg_to_int32 (state : Types.state) arg =
  match Assembler.Regs.of_string_opt arg with
  | Some s -> Arch.Cpu.get_reg state.arch.cpu (Int32.to_int s)
  | None ->
      match Assembler.Label.get_address_opt state.labels arg with
      | Some s -> s
      | None   -> Int32.of_string arg

let get_size labels label default =
  match Assembler.Label.get_size_opt labels label with
  | Some s -> s
  | None   -> default

