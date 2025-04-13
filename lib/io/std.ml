(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Common
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
let prompt = ref ""


let render (t : Cstring.t) start =
  let tl = String.length t.s in
  Tty.set_hcursor 0;
  Tty.erase_rcursor ();
  Tty.output start;
  Tty.output t.s;
  Tty.cursor_left (tl - t.cursor)

let add_history (t : t) s = { t with h = History.add t.h s }

let update op (t : t) =
  let s = op t.s in
  if s = t.s then Cont t else Cont { t with s }

let back     = update (Cstring.backspace 1)
let del      = update (Cstring.delete 1)
let move_end = update Cstring.move_end
let cleft  n = update (Cstring.move_left  n)
let cright n = update (Cstring.move_right n)
let move   n = update (Cstring.set_cursor n)
let tab    (t : t) = render t.s !prompt; Ret (Tab t)
let clear  (t : t) = Tty.clear_screen (); Cont t
let ctrl_d (t : t) = if Cstring.string t.s = "" then Ret Exit else del t
let break  (t : t) =
  if Cstring.string t.s = ""
  then Ret Exit
  else Cont { t with s = Cstring.empty }
let enter  (t : t) =
  let s = Cstring.string t.s in
  let t = add_history t s    in
  output "\n"; Ret (Line (Cstring.string t.s, t))

let hist_up (t : t) =
  match History.next t.h with
  | None   -> Cont t
  | Some h -> Cont { s = Cstring.create h.current; h }

let hist_down (t : t) =
  match History.prev t.h with
  | None   -> Cont t
  | Some h -> Cont { s = Cstring.create h.current; h }

let cmds_list : (Ansi.a * cmd) list = [
    Ctrl (Char 'a'),    move 0;
    Ctrl (Char 'e'),    move_end;
    Ctrl (Char 'c'),    break;
    Ctrl (Char 'd'),    ctrl_d;
    Ctrl (Char 'l'),    clear;
    Tab,                tab;
    Enter,              enter;
    Backspace,          back;
    Delete,             del;
    Arrow Up,           hist_up;
    Arrow Down,         hist_down;
    Arrow Left,         cleft  1;
    Arrow Right,        cright 1;
    Ctrl (Arrow Left),  update Cstring.prev_word;
    Ctrl (Arrow Right), update Cstring.next_word;
  ]

let init b =
  output "\x1b[5 q";
  List.iter (fun (c, f) -> Hashtbl.add cmds c f) cmds_list;
  prompt := b;
  Tty.init ()

let exit () =
  output "\x1b[0 q";
  Tty.exit ()

let rec read_line (t: t) : ret =
  let open Cstring in
  render t.s !prompt;
  match input () with
  | None          -> read_line t
  | Some (Char c) -> read_line { t with s = (add_char c t.s) }
  | Some c        -> exec_command c t

and exec_command c (t: t) : ret =
  match Hashtbl.find_opt cmds c with
  | None   -> read_line t
  | Some f ->
    match f t with
    | Cont t -> read_line t
    | Ret  s -> s

