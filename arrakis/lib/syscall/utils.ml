(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Simulator
open Types

let invalid_sc channel id =
  Format.fprintf channel
    "@{<fg_red>Error:@} @{<fg_yellow>'%d'@} Invalid syscall.@."
    (Int32.to_int id);
  Continue

(* File descriptors --------------------------------------------------------- *)

let cwd = ref (Unix.getcwd ())

let opened_fd : (int32, Unix.file_descr) Hashtbl.t= Hashtbl.create 3

let set_stdout stdin stdout stderr =
  List.iter (fun (k, v) -> Hashtbl.replace opened_fd k v)
  [
    0l, stdin;
    1l, stdout;
    2l, stderr;
  ]

let lfd = ref 3l

let close_fd fd =
  if fd > 2l then Hashtbl.remove opened_fd fd

let open_fd fd =
  lfd := Int32.add !lfd 1l;
  Hashtbl.add opened_fd !lfd fd;
  !lfd

(* Random ------------------------------------------------------------------- *)

let get_str_pointed_by (arch : Arch.t) adr =
  let res = ref "" in
  let adr = ref adr in
  let c   = ref (Memory.get_byte arch.memory !adr) in
  while !c <> 0l do
    res := Format.sprintf "%s%c" !res (Char.chr (Int32.to_int !c));
    adr := Int32.add !adr 1l;
    c   := Memory.get_byte arch.memory !adr;
  done;
  !res

let open_flag_list_from_int i =
  let open Unix in
  let (&) = Int32.logor in
  let fl =
    [
      00000000l, O_RDONLY;
      00000001l, O_WRONLY;
      00000002l, O_RDWR;
      00004000l, O_NONBLOCK;
      00002000l, O_APPEND;
      00000100l, O_CREAT;
      00001000l, O_TRUNC;
      00000200l, O_EXCL;
      00000400l, O_NOCTTY;
      00010000l, O_DSYNC;
      04000000l, O_SYNC;
      02000000l, O_CLOEXEC;
    ]
  in
  List.filter_map (fun (x, y) -> if (i&x) > 0l then Some y else None) fl

