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

let add_string (t : t) (s : string) =
  let tl = String.length t.s in
  let sl = String.length s   in
  if t.cursor = tl
  then { s = t.s ^ s; cursor = t.cursor + sl }
  else
    let b = befor t in
    let a = after   t in
    { s = b ^ s ^ a; cursor = t.cursor + sl }

let backspace (t : t) (len : int) =
  let tl = String.length t.s in
  if t.cursor < len
  then { s = String.sub t.s t.cursor (tl - t.cursor); cursor = 0 }
  else
    let b = String.sub t.s 0 (t.cursor - len) in
    let a = after t in
    { s = b ^ a; cursor = t.cursor - len }

let delete (t : t) (len : int) =
  let tl = String.length t.s in
  if t.cursor + len > tl
  then { t with s = befor t }
  else
    let b = befor t in
    let a = String.sub t.s (t.cursor + len) (tl - t.cursor - len) in
    { t with s = b ^ a }

let move_left (t: t) n =
  if t.cursor - n < 0
  then { t with cursor = 0 }
  else { t with cursor = t.cursor - n }

let move_right (t : t) n =
  let tl = String.length t.s in
  if t.cursor + n > tl
  then { t with cursor = tl }
  else { t with cursor = t.cursor + n }

