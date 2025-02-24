(* Arrakis ********************************************************************)
(* Copyright 2023-2025 Arrakis contributors                                   *)
(* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          *)
(******************************************************************************)

open Format

(* Useful character codes *)
let backspace = '\b'
let tab       = '\t'
let linefeed  = '\n'
let space     = ' '

(* Control sequence introducer *)
let csi = "\x1b["

(* Moving cursor *)
let cursor_up      n = sprintf "%s%dA" csi n
let cursor_down    n = sprintf "%s%dB" csi n
let cursor_forward n = sprintf "%s%dC" csi n
let cursor_back    n = sprintf "%s%dD" csi n

(* Display *)
let erase_display n = sprintf "%s%dJ" csi n
let erase_line    n = sprintf "%s%dJ" csi n
