(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Types
open Utils
open Simulator

let print_int channel (arch : Arch.t) =
  Format.fprintf channel "%d@." (Int32.to_int (Cpu.get_reg arch.cpu 11));
  Continue

let print_string channel (arch : Arch.t) =
  let adr = Cpu.get_reg arch.cpu 11     in
  let str = get_str_pointed_by arch adr in
  Format.fprintf channel "%s@." str;
  Continue

let print_character channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 11 in
  try
    let chr = Char.chr (Int32.to_int reg) in
    Format.fprintf channel "%c@." chr;
    Continue
  with _ ->
    Format.fprintf channel "@{<fg_red>Info:@} Syscall 'print_character' failed";
    Continue

let exit0 () = Exit 0

let sbrk =
  let heap_pointer = ref Simulator.Segment.heap_begin in
  fun (arch : Arch.t) ->
    let size = Simulator.Cpu.get_reg arch.cpu 11 in
    heap_pointer := Int32.add !heap_pointer size;
    Simulator.Cpu.set_reg arch.cpu 10 !heap_pointer;
    Continue

let exit (arch : Arch.t) =
  let status = Cpu.get_reg arch.cpu 11 in
  Exit (Int32.to_int status)

(* Source: https://github.com/ThaumicMekanism/venus/wiki/Environmental-Calls *)
let syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 10 in
  match reg with
  | 1l  -> print_int       channel arch
  | 4l  -> print_string    channel arch
  | 9l  -> sbrk                    arch
  | 10l -> exit0           ()
  | 11l -> print_character channel arch
  | 17l -> exit            arch
  | _   -> invalid_sc      channel reg

