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

let exit _  = Ret Exit
let tab   s = Ret (Tab s)
let enter s = Ret (Line (Cstring.string s))
let clear s = Tty.clear_screen (); Cont s
let back  s = Cont (Cstring.backspace s 1)

let cleft  n s = Cont (Cstring.move_left s n)
let cright n s = Cont (Cstring.move_right s n)

let cmds_list : (Ansi.a * cmd) list = [
    Ctrl (Char 'c'), exit;
    Ctrl (Char 'd'), exit;
    Ctrl (Char 'l'), clear;
    Tab,        tab;
    Enter,      enter;
    Backspace,  back;
    Arrow Left,  cleft  1;
    Arrow Right, cright 1;
  ]

let cmds = Hashtbl.create 16
let render_start = ref ""

let init b =
  output "\x1b[5 q";
  List.iter (fun (c, f) -> Hashtbl.add cmds c f) cmds_list;
  render_start := b;
  Tty.init ()

let exit () =
  output "\x1b[0 q";
  Tty.exit ()

let read_line () : ret =
  let rec loop (s: Cstring.t) =
    Cstring.render s !render_start;
    match input () with
    | None   -> loop s
    | Some c ->
      match c with
      | Char c -> loop (Cstring.add_string s (String.make 1 c))
      | _ ->
        match Hashtbl.find_opt cmds c with
        | None   -> Ansi.pp_ansi Format.std_formatter c; flush stdout; loop s
        | Some f ->
          match f s with
          | Cont s -> loop s
          | Ret s  -> s
  in
  loop Cstring.empty

