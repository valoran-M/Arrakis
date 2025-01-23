(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Tty

type t = {
  s : Cstring.t;
  h : History.t;
}

type ret =
  | Tab  of t
  | Line of string * t
  | Exit

type cmd_ret =
  | Cont of t
  | Ret  of ret

type cmd = t -> cmd_ret

let cmds = Hashtbl.create 16
let render_start = ref ""

let add_history (t : t) s = { t with h = History.add t.h s }

let tab    (t : t) = Cstring.render Cstring.empty !render_start; Ret (Tab t)
let clear  (t : t) = Tty.clear_screen (); Cont t
let back   (t : t) = Cont { t with s = Cstring.backspace t.s 1 }
let del    (t : t) = Cont { t with s = Cstring.delete    t.s 1 }
let ctrl_d (t : t) = if Cstring.string t.s = "" then Ret Exit else del t
let break  (t : t) =
  if Cstring.string t.s = ""
  then Ret Exit
  else Ret Exit
let enter  (t : t) =
  let s = Cstring.string t.s in
  let t = add_history t s    in
  output "\n"; Ret (Line (Cstring.string t.s, t))

let cleft  n (t : t) = Cont { t with s = Cstring.move_left  t.s n}
let cright n (t : t) = Cont { t with s = Cstring.move_right t.s n}

let hyst_up (t : t) =
  match History.next t.h with
  | None   -> Cont t
  | Some h -> Cont { s = Cstring.create h.current; h }

let hyst_down (t : t) =
  match History.prev t.h with
  | None   -> Cont t
  | Some h -> Cont { s = Cstring.create h.current; h }

let cmds_list : (Ansi.a * cmd) list = [
    Ctrl (Char 'c'),  break;
    Ctrl (Char 'd'),  ctrl_d;
    Ctrl (Char 'l'),  clear;
    Tab,              tab;
    Enter,            enter;
    Backspace,        back;
    Delete,           del;
    Arrow Up,         hyst_up;
    Arrow Down,       hyst_down;
    Arrow Left,       cleft  1;
    Arrow Right,      cright 1;
  ]

let init b =
  output "\x1b[5 q";
  List.iter (fun (c, f) -> Hashtbl.add cmds c f) cmds_list;
  render_start := b;
  Tty.init ()

let exit () =
  output "\x1b[0 q";
  Tty.exit ()

let read_line (t: t) : ret =
  let rec loop (t: t) =
    Cstring.render t.s !render_start;
    match input () with
    | None   -> loop t
    | Some c ->
      match c with
      | Char c -> loop { t with s = (Cstring.add_string t.s (String.make 1 c))}
      | _ ->
        match Hashtbl.find_opt cmds c with
        | None   -> Ansi.pp_ansi Format.std_formatter c; flush stdout; loop t
        | Some f ->
          match f t with
          | Cont t -> loop t
          | Ret s  -> s
  in
  loop t

