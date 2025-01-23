(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

external init  : unit -> bool = "caml_init_shell"
external exit  : unit -> bool = "caml_exit_shell"
external readc : unit -> int  = "caml_readc"
let readc () : int option =
  match readc () with
  | -1 | -2 -> None
  | -3      -> assert false
  | n       -> Some n

module Ansi = struct
  type d = Left | Up | Right | Down

  type a =
    | Tab | Enter | Backspace
    | Unknown of string
    | Arrow   of d
    | Char    of char
    | Ctrl    of a

  type e = Node of (int * e) list | Leaf of a

  let escape =
    Node [
      0x5B, (* [ *)
      Node [
        0x41, Leaf (Arrow Up);          (* [A *)
        0x42, Leaf (Arrow Down);        (* [B *)
        0x43, Leaf (Arrow Right);       (* [C *)
        0x44, Leaf (Arrow Left);        (* [D *)
      ];
    ]

  let pp = Format.fprintf

  let print_arrow ppf a =
    match a with
    | Up    -> pp ppf "up"
    | Down  -> pp ppf "down"
    | Right -> pp ppf "right"
    | Left  -> pp ppf "left"

  let rec pp_ansi ppf (a : a) =
    match a with
    | Tab       -> pp ppf "tab"
    | Enter     -> pp ppf "enter"
    | Backspace -> pp ppf "Backspace"
    | Unknown s -> pp ppf "unknown(%s)" s
    | Arrow a   -> pp ppf "arrow(%a)" print_arrow a
    | Char c    -> pp ppf "%c" c
    | Ctrl a    -> pp ppf "ctrl(%a)" pp_ansi a

  let read_escape () : a =
    let b = Buffer.create 2 in
    let (let*) o f =
      match o with
      | None   -> Unknown (Buffer.contents b)
      | Some i -> f i
    in
    let rec aux (e : e) (b : Buffer.t) =
      match e with
      | Leaf i -> i
      | Node l ->
        let* i = readc () in
        let* (_, e) = List.find_opt (fun (j, _) -> i = j) l in
        aux e b
    in
    aux escape b

  let input () : a option =
    match readc () with
    | None   -> None
    | Some i ->
      let a =
        match i with
        | 0x1b -> read_escape ()
        | 0x09 -> Tab
        | 0x0A -> Ctrl (Char '\n')
        | 0x0D -> Enter
        | 0x7F | 0x08 -> Backspace
        | c    ->
          if c <= 0x1F
          then Ctrl (Char (Char.chr (c + 0x60)))
          else Char (Char.chr c)
      in
      Some a
end

let init  = init
let exit  = exit
let input = Ansi.input
let output s = print_string s; flush stdout

let cursor_right c = if c <> 0 then output (Printf.sprintf "\x1b[%dC" c)
let cursor_left  c = if c <> 0 then output (Printf.sprintf "\x1b[%dD" c)

let set_hcursor c = output (Printf.sprintf "\x1b[%dG" c)

let erase_rcursor () = output "\x1b[0K"
let clear_screen  () = output "\x1b[2J\x1b[H"

