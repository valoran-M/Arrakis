(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

type t = { left : string list; current : string; right : string list }

let empty = { left = []; current = ""; right = [] }

let next (t : t) : t option =
  match t.right with
  | []     -> None
  | s :: r -> Some { left = t.current :: t.left; current = s; right = r }

let prev (t : t) : t option =
  match t.left with
  | []     -> None
  | s :: l -> Some { left = l; current = s; right = t.current :: t.right }

let add (t : t) (s : string) : t =
  { t with right = s :: t.right }

let reset (t : t) : t =
  { left = []; current = ""; right = (List.rev t.left) @ t.right}

