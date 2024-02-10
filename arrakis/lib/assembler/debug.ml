(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(*
  In the interface, we want to display information
  about the original program:

  - Line in the file where the code pointer is located
  - The original code before assembly

  This file contains all the functions needed to create and
  retrieve this information
*)

type t = {
  line_to_addr : (int,   int32)        Hashtbl.t;
  addr_to_line : (int32, int * string) Hashtbl.t
}

let generate_debug () =
  {
    line_to_addr = Hashtbl.create 32;
    addr_to_line = Hashtbl.create 32;
  }

let add_line_to_addr (debug : t) line addr =
  Hashtbl.replace (debug.line_to_addr) line addr

let get_addr (debug : t) line =
  Hashtbl.find (debug.line_to_addr) line

let add_addr_to_line (debug : t) addr line code =
  Hashtbl.replace (debug.addr_to_line) addr (line, code)

let get_line (debug : t) addr =
  Hashtbl.find (debug.addr_to_line) addr

