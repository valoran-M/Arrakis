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

let rec get_label_address_text (text : Program.text) labels addr =
  match text with
  | [] -> ()
(* No more pseudo instructions after remove_pseudo *)
  | Text_Pseudo _ :: _ -> assert false
  | Text_Instr  _ :: l -> get_label_address_text l labels (addr + 0x4l)
  | Text_GLabel _ :: l -> get_label_address_text l labels addr
  | Text_Label  s :: l ->
    add_address labels s addr;
    get_label_address_text l labels addr

let rec get_label_address_data (data : Program.data) labels addr =
  let open String in
  let open Int32 in
  match data with
  | [] -> ()
  | Data_Bytes bs :: l ->
    let new_addr = addr + Int32.of_int (List.length bs) in
    get_label_address_data l labels new_addr
  | Data_Ascii ls :: l ->
    let length = List.fold_left (fun l s -> of_int (length s) + l)  0l ls in
    get_label_address_data l labels (addr + length)
  | Data_Asciz ls :: l ->
    let length = List.fold_left (fun l s -> of_int (length s) + l + 1l) 0l ls in
    get_label_address_data l labels (addr + length)
  | Data_Word lw     :: l ->
    let offset = 0x4l * Int32.of_int (List.length lw) in
    get_label_address_data l labels (addr + offset)
  | Data_Zero nz     :: l -> get_label_address_data l labels (addr+4l*nz)
  | Data_GLabel _    :: l -> get_label_address_data l labels addr
  | Data_Label label :: l ->
    add_address labels label addr;
    get_label_address_data l labels addr

let get_label_address (prog : Program.t) =
  let open Arch.Segment in
  let labels =
    { label_to_address = Hashtbl.create 16;
      global_label     = Hashtbl.create 16; }
  in
  get_label_address_data prog.data labels data_begin;
  get_label_address_text prog.text labels text_begin;
  labels
