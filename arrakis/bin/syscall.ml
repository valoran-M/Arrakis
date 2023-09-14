open Simulator

type syscall_ret = Exit of int | Continue

(* Helper functions --------------------------------------------------------- *)

let invalid_sysc channel id =
  Format.fprintf channel
    "@{<fg_red>Error:@} @{<fg_yellow>'%d'@} Invalid syscall.@."
    (Int32.to_int id);
  Continue

let opened_fd = Hashtbl.create 3

let () =
  List.iter (fun (k, v) -> Hashtbl.add opened_fd k v)
  [
    0l,    Unix.stdin;
    1l,    Unix.stdout;
    2l,    Unix.stderr;
  ]

let lfd = ref 3l

let close_fd fd =
  if fd > 2l then Hashtbl.remove opened_fd fd

let open_fd fd =
  lfd := Int32.add !lfd 1l;
  Hashtbl.add opened_fd !lfd fd;
  !lfd

let get_str_pointed_by (arch : Arch.t) adr =
  let res = ref "" in
  let adr = ref adr in
  let c   = ref (Memory.get_byte arch.memory !adr) in
  while (!c <> 0l) do
    res := Format.sprintf "%s%c" !res (Char.chr (Int32.to_int !c));
    adr := Int32.add !adr 1l;
    c := Memory.get_byte arch.memory !adr;
  done;
  !res

(* Implementation of Venus ecall -------------------------------------------- *)

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
    Format.fprintf channel "@{<fg_red>Error:@} Couldn't print character.";
    Continue

let exit0 () = Exit 0

let sbrk _arch = failwith "TODO: sbrk"

let exit (arch : Arch.t) =
  let status = Cpu.get_reg arch.cpu 11 in
  Exit (Int32.to_int status)

(* Source: https://github.com/ThaumicMekanism/venus/wiki/Environmental-Calls *)
let venus_syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 10 in
  match reg with
  | 1l  -> print_int       channel arch
  | 4l  -> print_string    channel arch
  | 9l  -> sbrk                    arch
  | 10l -> exit0           ()
  | 11l -> print_character channel arch
  | 17l -> exit            arch
  | _   -> invalid_sysc    channel reg

(* Implementation of Linux ecall -------------------------------------------- *)

let exit (arch : Arch.t) =
  let status = Cpu.get_reg arch.cpu 10 in
  Exit (Int32.to_int status)

let kill (arch : Arch.t) =
  let pid = Cpu.get_reg arch.cpu 10    in
  let signal = Cpu.get_reg arch.cpu 11 in
  Unix.kill (Int32.to_int pid) (Int32.to_int signal);
  Continue

(* TODO : Move somewhere else *)
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
  let contained = List.filter
    (fun (x,_) -> (i&x) > 0l) fl
  in
  List.map (fun (_, y) -> y) contained

let openat (arch : Arch.t) =
  let _dfd  = Cpu.get_reg arch.cpu 10 in (* TODO: use? *)
  let adr   = Cpu.get_reg arch.cpu 11 in
  let flags = Cpu.get_reg arch.cpu 12 in
  let mode  = Cpu.get_reg arch.cpu 13 in

  let path  = get_str_pointed_by arch adr   in
  let flags = open_flag_list_from_int flags in

  let fd = open_fd (Unix.openfile path flags (Int32.to_int mode)) in

  Cpu.set_reg arch.cpu 10 fd;
  Continue

let close (arch : Arch.t) =
  let fd = Cpu.get_reg arch.cpu 10 in
  close_fd fd;
  Continue

let read (arch : Arch.t) =
  let fd    = Cpu.get_reg arch.cpu 10 in
  let buf   = Cpu.get_reg arch.cpu 11 in
  let count = Cpu.get_reg arch.cpu 12 in

  let fd  = Hashtbl.find opened_fd fd in
  let mem = Memory.direct_access arch.memory in
  let res = Unix.read fd mem (Int32.to_int buf) (Int32.to_int count) in

  Cpu.set_reg arch.cpu 10 (Int32.of_int res);
  Continue

let write (arch : Arch.t) =
  let fd    = Cpu.get_reg arch.cpu 10 in
  let buf   = Cpu.get_reg arch.cpu 11 in
  let count = Cpu.get_reg arch.cpu 12 in

  let fd  = Hashtbl.find opened_fd fd in
  let mem = Memory.direct_access arch.memory in
  let res = Unix.write fd mem (Int32.to_int buf) (Int32.to_int count) in

  Cpu.set_reg arch.cpu 10 (Int32.of_int res);
  Continue

let sbrk (_arch : Arch.t) =
  failwith "TODO: sbrk"

let getuid (arch : Arch.t) =
  let uid = Unix.getuid () in
  Cpu.set_reg arch.cpu 10 (Int32.of_int uid);
  Continue

let geteuid (arch : Arch.t) =
  let euid = Unix.geteuid () in
  Cpu.set_reg arch.cpu 10 (Int32.of_int euid);
  Continue

let getcwd (arch : Arch.t) =
  let buf  = Cpu.get_reg arch.cpu 10 in
  let size = Cpu.get_reg arch.cpu 11 in
  let str  = Unix.getcwd ()          in

  (
    try Memory.set_str arch.memory buf str (Int32.to_int size)
    with _ ->
      Format.eprintf "@{<fg_blue>Info:@} Syscall getcwd failed@.";
      Cpu.set_reg arch.cpu 10 0l
  );
  Continue

let chdir (arch : Arch.t) =
  let path = Cpu.get_reg arch.cpu 10      in
  let path = get_str_pointed_by arch path in

  (
    try Unix.chdir path;
    with _ ->
      Format.eprintf "@{<fg_blue>Info:@} Syscall chdir failed@.";
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
let unix_syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 17 in
  match reg with
  | 17l  -> getcwd    arch
  | 34l  -> mkdirat   arch
  | 49l  -> chdir     arch
  | 56l  -> openat    arch
  | 57l  -> close     arch
  | 63l  -> read      arch
  | 64l  -> write     arch
  | 93l  -> exit      arch
  | 129l -> kill      arch
  | 174l -> getuid    arch
  | 175l -> geteuid   arch
  | 214l -> sbrk      arch
  | _    -> invalid_sysc channel reg

(* -------------------------------------------------------------------------- *)

let syscall =
  match Options.env with
  | "unix"  -> unix_syscall
  | "venus" -> venus_syscall
  | _       -> failwith "Invalid environment."

