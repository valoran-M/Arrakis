open Simulator

type syscall_ret = Exit of int | Continue

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

(* Implementation of various SysCalls --------------------------------------- *)

let print_int channel (arch : Arch.t) =
  Format.fprintf channel "%d@." (Int32.to_int (Cpu.get_reg arch.cpu 11));
  Continue

let print_string _channel _arch = failwith "TODO: print_string"

let print_character channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 11 in
  try
    let chr = Char.chr (Int32.to_int reg) in
    Format.fprintf channel "%c@." chr;
    Continue
  with _ -> failwith "Too big" (* TODO: Better error here. *)

let exit0 () = Exit 0

let exit2 (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 11 in
  Exit (Int32.to_int reg)

let exit (arch : Arch.t) =
  let status = Cpu.get_reg arch.cpu 11 in
  Exit (Int32.to_int status)

let sbrk _arch = failwith "TODO: sbrk"

let kill (arch : Arch.t) =
  let pid = Cpu.get_reg arch.cpu 11 in
  let signal = Cpu.get_reg arch.cpu 12 in
  Unix.kill (Int32.to_int pid) (Int32.to_int signal);
  Continue

let open_file (_arch : Arch.t) =
  failwith "todo: open"

let close (_arch : Arch.t) =
  failwith "todo: close"

let read (_arch : Arch.t) =
  failwith "todo : read"

let write (_arch : Arch.t) =
  failwith "todo : write"

let getuid (arch : Arch.t) =
  let uid = Unix.getuid () in
  Cpu.set_reg arch.cpu 11 (Int32.of_int uid);
  Continue

let geteuid (arch : Arch.t) =
  let euid = Unix.geteuid () in
  Cpu.set_reg arch.cpu 11 (Int32.of_int euid);
  Continue

let time (arch : Arch.t) =
  let t = Unix.time () in
  Cpu.set_reg arch.cpu 11 (Int32.of_float t);
  Continue

(* --- *)

(* Source: https://github.com/ThaumicMekanism/venus/wiki/Environmental-Calls *)
let venus_syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 10 in
  match reg with
  | 1l  -> print_int       channel arch
  | 4l  -> print_string    channel arch
  | 9l  -> sbrk                    arch
  | 10l -> exit0           ()
  | 11l -> print_character channel arch
  | 13l -> open_file       arch
  | 14l -> read            arch
  | 15l -> write           arch
  | 16l -> close           arch
  | 17l -> exit            arch
  | _   -> invalid_sysc    channel reg

(* Source: https://jborza.com/post/2021-05-11-riscv-linux-syscalls/ *)
let unix_syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 10 in
  match reg with
  | 17l  -> failwith "todo: getcwd"
  | 37l  -> failwith "todo: link"
  | 34l  -> failwith "todo: mkdirat"
  | 35l  -> failwith "todo: unlinkat"
  | 49l  -> failwith "todo: chdir"
  | 56l  -> open_file arch
  | 57l  -> close     arch
  | 63l  -> read      arch
  | 64l  -> write     arch
  | 93l  -> exit   arch
  | 129l -> kill   arch
  | 174l -> getuid arch
  | 175l -> geteuid arch
  | 221l -> failwith "todo: execve"
  | 214l -> failwith "todo: brk"
  | _    -> invalid_sysc channel reg

let syscall =
  match Options.env with
  | "unix"  -> unix_syscall
  | "venus" -> venus_syscall
  | _       -> failwith "Invalid environment."
