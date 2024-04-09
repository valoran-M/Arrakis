(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

(*
  In assembler code there are labels which are strings of characters
  representing addresses.

  You can also declare global labels. So we want a list of global labels.
*)

open Error
open Program

let ( * ) = Int32.mul
let ( + ) = Int32.add
let ( - ) = Int32.sub

type t = {
    label_to_address : (string, int32) Hashtbl.t;
    global_label     : (string, int32) Hashtbl.t;
  }

let add_address (labels : t) label addr =
  Hashtbl.replace labels.label_to_address label addr

let get_address (labels : t) label line =
  match Hashtbl.find_opt labels.label_to_address label with
  | None      -> raise (Assembler_error (line, (Error.Unknown_Label label)))
  | Some addr -> addr

let get_address_opt (labels : t) label =
  Hashtbl.find_opt labels.label_to_address label

let made_global (labels : t) label line =
  match Hashtbl.find_opt labels.label_to_address label with
  | None      -> raise (Assembler_error (line, (Error.Unknown_Label label)))
  | Some addr -> Hashtbl.replace labels.global_label label addr

let get_global (labels : t) label =
  Hashtbl.find_opt labels.global_label label

(* Assembly ----------------------------------------------------------------- *)

let rec get_label_address_program prog labels addr =
  match prog with
  | [] -> ()
  | Text_Pseudo (_, _, instruction) :: l ->
    let new_addr = addr + Instructions.Pseudo.pseudo_length instruction in
    get_label_address_program l labels new_addr
  | Text_Instr (_,_,_)::l -> get_label_address_program l labels (addr + 0x4l)
  | Text_GLabel _::l      -> get_label_address_program l labels addr
  | Text_Label label::l ->
    add_address labels label addr;
    get_label_address_program l labels addr

let rec get_label_address_memory (memory : data_line list) labels addr =
  match memory with
  | [] -> ()
  | Data_Bytes bs :: l ->
    let new_addr = addr + Int32.of_int (List.length bs) in
    get_label_address_memory l labels new_addr
  | Data_Ascii s :: l ->
    let new_addr = addr + Int32.of_int (String.length s) in
    get_label_address_memory l labels new_addr
  | Data_Asciz s :: l ->
    let new_addr = addr + Int32.of_int (String.length s) + 1l in
    get_label_address_memory l labels new_addr
  | Data_Word lw     :: l ->
    let offset = 0x4l * Int32.of_int (List.length lw) in
    get_label_address_memory l labels (addr + offset)
  | Data_Zero nz     :: l -> get_label_address_memory l labels (addr+4l*nz)
  | Data_GLabel _    :: l -> get_label_address_memory l labels addr
  | Data_Label label :: l ->
    add_address labels label addr;
    get_label_address_memory l labels addr

let get_label_address (prog : program) =
  let open Arch.Segment in
  let labels =
    {
      label_to_address = Hashtbl.create 16;
      global_label     = Hashtbl.create 16;
    }
  in
  get_label_address_memory  prog.memory  labels static_begin;
  get_label_address_program prog.program labels text_begin;
  labels

