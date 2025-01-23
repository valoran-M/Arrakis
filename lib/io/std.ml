(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Tty

module CString = struct
  type t = { s : string; cursor : int }

  let empty = { s = ""; cursor = 0 }

  let _begin (t: t) = String.sub t.s 0 t.cursor
  let _end (t: t) =
    let tl = String.length t.s in
    String.sub t.s t.cursor (tl - t.cursor)

  let add_string (t : t) (s : string) =
    let tl = String.length t.s in
    let sl = String.length s   in
    if t.cursor = tl
    then { s = t.s ^ s; cursor = t.cursor + sl }
    else
      let b = _begin t in
      let e = _end   t in
      { s = b ^ s ^ e; cursor = t.cursor + sl }

  let delete (t: t) (len : int) =
    let tl = String.length t.s in
    if t.cursor < len
    then { s = String.sub t.s t.cursor (tl - t.cursor); cursor = 0 }
    else
      let b = String.sub t.s 0 (t.cursor - len) in
      let e = _end t in
      { s = b ^ e; cursor = t.cursor - len }

  let move_left (t: t) n =
    if t.cursor - n < 0
    then { t with cursor = 0 }
    else { t with cursor = t.cursor - n }

  let move_right (t: t) n =
    let tl = String.length t.s in
    if t.cursor + n > tl
    then { t with cursor = tl }
    else { t with cursor = t.cursor + n }

  let render (t: t) =
    let tl = String.length t.s in
    Tty.set_hcursor 0;
    Tty.erase_rcursor ();
    output "> ";
    output t.s;
    cursor_left (tl - t.cursor)
end

type ret =
  | Tab  of CString.t
  | Line of string
  | Exit

let init = Tty.init
let exit = Tty.exit

let read_line () : ret =
  let rec loop (s: CString.t) =
    CString.render s;
    match input () with
    | None   -> loop s
    | Some c ->
      match c with
      | Ctrl (Char 'c')
      | Ctrl (Char 'd') -> Exit
      | Ctrl (Char 'l') -> Tty.clear_screen (); loop s
      | Ansi.Tab        -> Tab s
      | Enter           -> output "\n\r"; Line (s.s)
      | Char c          -> loop (CString.add_string s (String.make 1 c))
      | Backspace       -> loop (CString.delete s 1)
      | Arrow (Left)    -> loop (CString.move_left s 1)
      | Arrow (Right)   -> loop (CString.move_right s 1)
      | c -> Ansi.pp_ansi Format.std_formatter c; flush stdout; loop s
  in
  output "\x1b[5 q";
  let r = loop CString.empty in
  output "\x1b[0 q";
  r

