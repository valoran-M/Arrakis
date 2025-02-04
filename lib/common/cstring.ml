(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type t = { s : string; cursor : int }

let empty = { s = ""; cursor = 0 }

let create s = { s; cursor = String.length s }

let befor (t : t) = String.sub t.s 0 t.cursor
let after (t : t) =
  let tl = String.length t.s in
  String.sub t.s t.cursor (tl - t.cursor)

let string s = s.s

let length s = String.length (string s)

let add_string (s : string) (t : t) =
  let tl = String.length t.s in
  let sl = String.length s   in
  if t.cursor = tl
  then { s = t.s ^ s; cursor = t.cursor + sl }
  else
    let b = befor t in
    let a = after   t in
    { s = b ^ s ^ a; cursor = t.cursor + sl }

let add_char c (t : t) = add_string (String.make 1 c) t

let backspace (len : int) (t : t) =
  let tl = String.length t.s in
  if t.cursor < len
  then { s = String.sub t.s t.cursor (tl - t.cursor); cursor = 0 }
  else
    let b = String.sub t.s 0 (t.cursor - len) in
    let a = after t in
    { s = b ^ a; cursor = t.cursor - len }

let delete (len : int) (t : t) =
  let tl = String.length t.s in
  if t.cursor + len > tl
  then { t with s = befor t }
  else
    let b = befor t in
    let a = String.sub t.s (t.cursor + len) (tl - t.cursor - len) in
    { t with s = b ^ a }

let set_cursor cursor t =
  if length t < cursor
  then t
  else { t with cursor }

let move_left n (t : t) =
  if t.cursor - n < 0
  then { t with cursor = 0 }
  else { t with cursor = t.cursor - n }

let move_right n (t : t) =
  let tl = String.length t.s in
  if t.cursor + n > tl
  then { t with cursor = tl }
  else { t with cursor = t.cursor + n }

let move_end (t : t) = set_cursor (length t) t

let prev_word (t : t) =
  let rec loop i min =
    if i <= min || t.s.[i] = ' '
    then i
    else loop (i - 1) min
  in
  let c =
    if t.cursor = length t || t.s.[t.cursor] = ' '
    then t.cursor - 1
    else t.cursor
  in
  { t with cursor = loop c 0 }

let next_word (t : t) =
  let rec loop i max =
    if i >= max || t.s.[i] = ' '
    then i
    else loop (i + 1) max
  in
  let l = length t in
  let c =
    if t.cursor <> l && t.s.[t.cursor] = ' '
    then t.cursor + 1
    else t.cursor
  in
  let c = loop c l in
  let cursor =  if c = l then c else c + 1 in
  { t with cursor }
  
