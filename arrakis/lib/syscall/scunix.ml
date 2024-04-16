(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Types
open Arch
open Utils
open Global_utils.Print
open Global_utils.Integer

let (>) x y = Int32.unsigned_compare x y > 0

let exit (arch : Riscv.t) =
  let status = Cpu.get_reg arch.cpu 10 in
  Exit (Int32.to_int status)

let kill (arch : Riscv.t) =
  let pid    = Cpu.get_reg arch.cpu 10 |> Int32.to_int in
  let signal = Cpu.get_reg arch.cpu 11 |> Int32.to_int in
  Unix.kill pid signal;
  Continue

let openat channel (arch : Riscv.t) =
  let dirfd = Cpu.get_reg arch.cpu 10 in
  let addr  = Cpu.get_reg arch.cpu 11 in
  let flags = Cpu.get_reg arch.cpu 12 in
  let mode  = Cpu.get_reg arch.cpu 13 in

  let path  = get_str_pointed_by arch addr  in
  let flags = open_flag_list_from_int flags in

  try
    let path =
      if (String.get path 0 = '/') then path
      else if dirfd = -100l        then !cwd ^ path
      else
        (* TODO:
          Path is supposed to be relative to dirfd.
          Failure to avoid incorrect semantics
        *)
        raise (Failure "#") (* Will be catched *)
    in
    let fd = open_fd (Unix.openfile path flags (Int32.to_int mode)) in
    Cpu.set_reg arch.cpu 10 fd;
    Continue
  with _ ->
    let open Format in
    Cpu.set_reg arch.cpu 10 (-1l);
    fprintf channel
      "%a Syscall @{<fg_yellow>'openat'@} failed@." info ();
    Continue

let close (arch : Riscv.t) =
  Cpu.get_reg arch.cpu 10
  |> close_fd;
  Continue

let read channel (arch : Riscv.t) =
  let fd    = Cpu.get_reg arch.cpu 10 in
  let buf   = Cpu.get_reg arch.cpu 11 in
  let count = Cpu.get_reg arch.cpu 12 in

  try
    let fd  = Hashtbl.find opened_fd fd in
    let mem = Memory.direct_access arch.memory in
    let res = Unix.read fd mem (Int32.to_int buf) (Int32.to_int count - 1) in
    Memory.set_byte arch.memory (Int32.add (Int32.of_int res) buf) 0l;
    Cpu.set_reg arch.cpu 10 (Int32.of_int (res + 1));
    Continue
  with
  | Not_found ->
    let open Format in
    Cpu.set_reg arch.cpu 10 (-1l);
    fprintf channel
      "%a Syscall @{<fg_yellow>'read'@} failed: Reading in unopened file descriptor.@."
      info ();
    Continue
  | Invalid_argument _ ->
    let open Format in
    Cpu.set_reg arch.cpu 10 (-1l);
    fprintf channel "%a Syscall @{<fg_yellow>'write'@} failed: Invalid argument@."
      info ();
    Continue

let write channel (arch : Riscv.t) =
  let fd    = Cpu.get_reg arch.cpu 10 in
  let buf   = Cpu.get_reg arch.cpu 11 in
  let count = Cpu.get_reg arch.cpu 12 in

  try
    let fd  = Hashtbl.find opened_fd fd in
    let mem = Memory.direct_access arch.memory in
    let res = Unix.write fd mem (int32_to_int buf) (int32_to_int count) in
    Cpu.set_reg arch.cpu 10 (Int32.of_int res);
    Continue
  with
  | Not_found ->
    let open Format in
    Cpu.set_reg arch.cpu 10 (-1l);
    fprintf channel
      "%a Syscall @{<fg_yellow>'write'@} failed: Writing in unopened file descriptor@."
      info ();
    Continue
  | Invalid_argument _ ->
    let open Format in
    Cpu.set_reg arch.cpu 10 (-1l);
    fprintf channel
      "%a Syscall @{<fg_yellow>'write'@} failed: Invalid argument@."
      info ();
    Continue

let brk (arch : Riscv.t) =
  let new_addr   = Cpu.get_reg arch.cpu 10 in
  let stack_addr = Cpu.get_reg arch.cpu 2  in
  let success =
    if new_addr < stack_addr && new_addr >= Segment.heap_begin
    then 0l
    else -1l
  in
  Cpu.set_reg arch.cpu 10 success;
  Continue

let getuid (arch : Riscv.t) =
  Unix.getuid ()
  |> Int32.of_int
  |> Cpu.set_reg arch.cpu 10;
  Continue

let geteuid (arch : Riscv.t) =
  Unix.geteuid ()
  |> Int32.of_int
  |> Cpu.set_reg arch.cpu 10;
  Continue

let getcwd channel (arch : Riscv.t) =
  let buf  = Cpu.get_reg arch.cpu 10 in
  let size = Cpu.get_reg arch.cpu 11 in
  let str  = !cwd in
  (
    try
      (* TODO: Give out proper error to user here *)
      let str_size = (String.length str) + 1 in
      (if str_size >= (Int32.to_int size) then raise (Failure ""));
      ignore (Memory.set_strz arch.memory buf str (Int32.to_int size));
      Cpu.set_reg arch.cpu 10 (Int32.of_int str_size)
    with _ ->
      Format.fprintf channel "%a Syscall @{<fg_yellow>'getcwd'@} failed@."
        info ();
      Cpu.set_reg arch.cpu 10 0l
  );
  Continue

let chdir (arch : Riscv.t) =
  let path = Cpu.get_reg arch.cpu 10      in
  let path = get_str_pointed_by arch path in
  cwd := path;
  Continue

let mkdirat (arch : Riscv.t) =
  let pathname = Cpu.get_reg arch.cpu 10 in
  let mode     = Cpu.get_reg arch.cpu 11 in

  let pathname = get_str_pointed_by arch pathname in
  Unix.mkdir pathname (Int32.to_int mode); (* TODO: Handle failure *)
  Continue

(* Source: https://jborza.com/post/2021-05-11-riscv-linux-syscalls/ *)
let syscall channel (arch : Riscv.t) =
  let reg = Cpu.get_reg arch.cpu 17 in
  match reg with
  | 17l  -> getcwd     channel arch
  | 34l  -> mkdirat            arch
  | 49l  -> chdir              arch
  | 56l  -> openat     channel arch
  | 57l  -> close              arch
  | 63l  -> read       channel arch
  | 64l  -> write      channel arch
  | 93l  -> exit               arch
  | 129l -> kill               arch
  | 174l -> getuid             arch
  | 175l -> geteuid            arch
  | 214l -> brk                arch
  | _    -> invalid_sc channel reg

