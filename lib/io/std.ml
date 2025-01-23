(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Tty

type ret =
  | Tab  of Cstring.t
  | Line of string
  | Exit

let render_start = ref ""

let init b =
  output "\x1b[5 q";
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
      | Ctrl (Char 'c')
      | Ctrl (Char 'd') -> Exit
      | Ctrl (Char 'l') -> Tty.clear_screen (); loop s
      | Ansi.Tab        -> Tab s
      | Enter           -> output "\n"; Line (s.s)
      | Char c          -> loop (Cstring.add_string s (String.make 1 c))
      | Backspace       -> loop (Cstring.delete s 1)
      | Arrow (Left)    -> loop (Cstring.move_left s 1)
      | Arrow (Right)   -> loop (Cstring.move_right s 1)
      | c -> Ansi.pp_ansi Format.std_formatter c; flush stdout; loop s
  in
  loop Cstring.empty

