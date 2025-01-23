(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Tty

type ret =
  | Tab  of Cstring.t
  | Line of string
  | Exit

type cmd_ret =
  | Cont of Cstring.t
  | Ret  of ret

type cmd = Cstring.t -> cmd_ret

type t = {
  s : Cstring.t;
  h : History.t;
}

let cmds = Hashtbl.create 16
let render_start = ref ""

let break  _ = output "\n"; Ret Exit
let tab    s = Cstring.render Cstring.empty !render_start; Ret (Tab s)
let enter  s = output "\n"; Ret (Line (Cstring.string s))
let clear  s = Tty.clear_screen (); Cont s
let back   s = Cont (Cstring.backspace s 1)
let del    s = Cont (Cstring.delete s 1)
let ctrl_d s = if Cstring.string s = "" then break s else del s

let cleft  n s = Cont (Cstring.move_left s n)
let cright n s = Cont (Cstring.move_right s n)

let cmds_list : (Ansi.a * cmd) list = [
    Ctrl (Char 'c'),  break;
    Ctrl (Char 'd'),  ctrl_d;
    Ctrl (Char 'l'),  clear;
    Tab,              tab;
    Enter,            enter;
    Backspace,        back;
    Delete,           del;
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
          match f t.s with
          | Cont s -> loop { t with s }
          | Ret s  -> s
  in
  loop t

