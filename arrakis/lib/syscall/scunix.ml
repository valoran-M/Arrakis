open Types
open Utils
open Simulator

let (>)  x y = Int32.unsigned_compare x y >  0

let exit (arch : Arch.t) =
  let status = Cpu.get_reg arch.cpu 10 in
  Exit (Int32.to_int status)

let kill (arch : Arch.t) =
  let pid = Cpu.get_reg arch.cpu 10    in
  let signal = Cpu.get_reg arch.cpu 11 in
  Unix.kill (Int32.to_int pid) (Int32.to_int signal);
  Continue

let openat (arch : Arch.t) =
  (* TODO: Use dirfd.
     If the pathname given in pathname is absolute, dirfd is ignored.
     If the pathname given in pathname is relative and pathname is the special
     value AT_FDCXD then pathname is interpreted relative to current working
     directory.
     Otherwise it is interpreted as relative to dirfd.
  *)
  let _dirfd  = Cpu.get_reg arch.cpu 10 in
  let adr   = Cpu.get_reg arch.cpu 11   in
  let flags = Cpu.get_reg arch.cpu 12   in
  let mode  = Cpu.get_reg arch.cpu 13   in

  let path  = get_str_pointed_by arch adr   in
  let flags = open_flag_list_from_int flags in

  let fd = open_fd (Unix.openfile path flags (Int32.to_int mode)) in

  Cpu.set_reg arch.cpu 10 fd;
  Continue

let close (arch : Arch.t) =
  let fd = Cpu.get_reg arch.cpu 10 in
  close_fd fd;
  Continue

let read channel (arch : Arch.t) =
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
  with Not_found ->
    Cpu.set_reg arch.cpu 10 (-1l);
    Format.fprintf channel
      "@{<fg_red>Error:@} Reading in unopened file descriptor.@.";
    Continue

let write channel (arch : Arch.t) =
    let fd    = Cpu.get_reg arch.cpu 10 in
    let buf   = Cpu.get_reg arch.cpu 11 in
    let count = Cpu.get_reg arch.cpu 12 in

    try
      let fd  = Hashtbl.find opened_fd fd in
      let mem = Memory.direct_access arch.memory in
      let res = Unix.write fd mem (Int32.to_int buf) (Int32.to_int count) in

      Cpu.set_reg arch.cpu 10 (Int32.of_int res);
      Continue
    with Not_found ->
      Cpu.set_reg arch.cpu 10 (-1l);
      Format.fprintf channel
        "@{<fg_red>Error:@} Writing in unopened file descriptor.@.";
      Continue

let brk (arch: Arch.t) =
  let new_addr   = Simulator.Cpu.get_reg arch.cpu 10 in
  let stack_addr = Simulator.Cpu.get_reg arch.cpu 2 in
  (if new_addr < stack_addr && new_addr >= Simulator.Segment.heap_begin
   then Cpu.set_reg arch.cpu 10 0l
   else Cpu.set_reg arch.cpu 10 (-1l)
  );

  Continue

let getuid (arch : Arch.t) =
  let uid = Unix.getuid () in
  Cpu.set_reg arch.cpu 10 (Int32.of_int uid);
  Continue

let geteuid (arch : Arch.t) =
  let euid = Unix.geteuid () in
  Cpu.set_reg arch.cpu 10 (Int32.of_int euid);
  Continue

let getcwd channel (arch : Arch.t) =
  let buf  = Cpu.get_reg arch.cpu 10 in
  let size = Cpu.get_reg arch.cpu 11 in
  let str  = Unix.getcwd ()          in
  (
    try Memory.set_str arch.memory buf str (Int32.to_int size)
    with _ ->
      Format.fprintf channel "@{<fg_blue>Info:@} Syscall getcwd failed@.";
      Cpu.set_reg arch.cpu 10 0l
  );
  Continue

let chdir channel (arch : Arch.t) =
  let path = Cpu.get_reg arch.cpu 10      in
  let path = get_str_pointed_by arch path in

  (
    try Unix.chdir path;
    with _ ->
      Format.fprintf channel "@{<fg_blue>Info:@} Syscall chdir failed@.";
      Cpu.set_reg arch.cpu 10 (Int32.of_int (-1));
  );
  Continue

let mkdirat (arch : Arch.t) =
  let pathname = Cpu.get_reg arch.cpu 10 in
  let mode = Cpu.get_reg arch.cpu 11 in

  let pathname = get_str_pointed_by arch pathname in
  Unix.mkdir pathname (Int32.to_int mode);
  Continue

(* Source: https://jborza.com/post/2021-05-11-riscv-linux-syscalls/ *)
let syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 17 in
  match reg with
  | 17l  -> getcwd    channel arch
  | 34l  -> mkdirat           arch
  | 49l  -> chdir     channel arch
  | 56l  -> openat            arch
  | 57l  -> close             arch
  | 63l  -> read      channel arch
  | 64l  -> write     channel arch
  | 93l  -> exit              arch
  | 129l -> kill              arch
  | 174l -> getuid            arch
  | 175l -> geteuid           arch
  | 214l -> brk               arch
  | _    -> invalid_sysc channel reg

