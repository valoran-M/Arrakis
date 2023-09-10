open Simulator

type syscall_ret = Exit of int | Continue

(* Helper functions --------------------------------------------------------- *)

let invalid_sysc channel reg =
  Format.fprintf channel
    "@{<fg_red>Error:@} @{<fg_yellow>'%d'@} Invalid syscall.@."
    (Int32.to_int reg);
  Continue

let opened_fd = Hashtbl.create 3

let () =
  List.iter (fun (k, v) -> Hashtbl.add opened_fd k v)
  [
    0,    Unix.stdin;
    1,    Unix.stdout;
    2,    Unix.stderr;
  ]

let close_fd fd =
  if fd > 2 then Hashtbl.remove opened_fd fd

let get_str_pointed_by (arch : Arch.t) adr =
  let res = ref "" in
  let adr = ref adr in
  let c   = ref (Memory.get_byte arch.memory !adr) in
  while (!c != 0l) do
    res := Format.sprintf "%s%c" !res (Char.chr (Int32.to_int !c));
    adr := Int32.add !adr 1l;
    c := Memory.get_byte arch.memory !adr
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
  with _ -> failwith "Too big" (* TODO: Better error here. *)

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

let openat (arch : Arch.t) =
  let _dfd   = Cpu.get_reg arch.cpu 10     in
  let _adr   = Cpu.get_reg arch.cpu 11     in
  let _flags = Cpu.get_reg arch.cpu 12     in
  let _mode  = Cpu.get_reg arch.cpu 13     in

  let _path  = get_str_pointed_by arch _adr in

  failwith "todo: open"

let close (_arch : Arch.t) =
  failwith "todo: close"

let read (_arch : Arch.t) =
  failwith "todo : read"

let write (_arch : Arch.t) =
  failwith "todo : write"

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

let execve (_arch : Arch.t) =
  failwith "todo: execve"

(* Source: https://jborza.com/post/2021-05-11-riscv-linux-syscalls/ *)
let unix_syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 17 in
  match reg with
  | 17l  -> failwith "todo: getcwd"
  | 34l  -> failwith "todo: mkdirat"
  | 35l  -> failwith "todo: unlinkat"
  | 37l  -> failwith "todo: link"
  | 49l  -> failwith "todo: chdir"
  | 56l  -> openat    arch
  | 57l  -> close     arch
  | 63l  -> read      arch
  | 64l  -> write     arch
  | 93l  -> exit      arch
  | 129l -> kill      arch
  | 174l -> getuid    arch
  | 175l -> geteuid   arch
  | 221l -> execve    arch
  | 214l -> sbrk      arch
  | _    -> invalid_sysc channel reg

(* -------------------------------------------------------------------------- *)

let syscall =
  match Options.env with
  | "unix"  -> unix_syscall
  | "venus" -> venus_syscall
  | _       -> failwith "Invalid environment."

