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