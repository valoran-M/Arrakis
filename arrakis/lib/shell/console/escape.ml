(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
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
let display_erase  n = sprintf "%s%dJ" csi n
