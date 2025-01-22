(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Tty

type ret =
  | Tab  of string (* entry for autocompletion *)
  | Line of string
  | Exit

let init = Tty.init
let exit = Tty.exit

let read_line () : ret =
  let b = Buffer.create 8 in
  let rec loop () =
    match input () with
    | None   -> loop ()
    | Some c ->
      match c with
      | Ctrl (Char 'c') -> Exit
      | Enter           -> output "\n\r"; Line (Buffer.contents b)
      | Char c          ->
        Buffer.add_char b c;
        print_char c; flush stdout; loop ()
      | Backspace       ->
        Buffer.truncate b (Buffer.length b - 1);
        output "\b"; loop ()
      | c -> Ansi.pp_ansi Format.std_formatter c; flush stdout; loop ()
  in
  loop ()

