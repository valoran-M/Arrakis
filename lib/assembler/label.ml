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
  label_size       : (string, int)   Hashtbl.t;
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

let set_size (labels : t) label size =
  Hashtbl.replace labels.label_size label size

let get_size     (labels : t) label = Hashtbl.find     labels.label_size label
let get_size_opt (labels : t) label = Hashtbl.find_opt labels.label_size label

(* Assembly ----------------------------------------------------------------- *)

let get_addr_directive (directive : Program.global_directive) addr =
  match directive with
  | _ -> addr

let rec get_label_text (text : Program.text) labels addr =
  match text with
  | [] -> ()
  | Text_Instr  _ :: l -> get_label_text l labels (addr + 0x4l)
  | Text_Direc  d :: l -> get_label_text l labels (get_addr_directive d addr)
  | Text_Label  s :: l ->
    add_address labels s addr;
    get_label_text l labels addr
(* No more pseudo instructions after remove_pseudo *)
  | Text_Pseudo _ :: _ -> assert false

let rec get_label_data (data : Program.data) labels addr =
  let open String in
  let open Int32 in
  match data with
  | [] -> ()
  | Data_Bytes bs :: l ->
    let new_addr = addr + Int32.of_int (List.length bs) in
    get_label_data l labels new_addr
  | Data_Ascii ls :: l ->
    let length = List.fold_left (fun l s -> of_int (length s) + l)  0l ls in
    get_label_data l labels (addr + length)
  | Data_Asciz ls :: l ->
    let length = List.fold_left (fun l s -> of_int (length s) + l + 1l) 0l ls in
    get_label_data l labels (addr + length)
  | Data_Word lw :: l ->
    let offset = 0x4l * Int32.of_int (List.length lw) in
    get_label_data l labels (addr + offset)
  | Data_Zero nz  :: l -> get_label_data l labels (addr+4l*nz)
  | Data_Direc d  :: l -> get_label_data l labels (get_addr_directive d addr)
  | Data_Label lb :: l ->
    add_address labels lb addr;
    get_label_data l labels addr

let get_label_address (prog : Program.t) =
  let open Arch.Segment in
  let labels =
    { label_size       = Hashtbl.create 8;
      label_to_address = Hashtbl.create 16;
      global_label     = Hashtbl.create 16; }
  in
  get_label_data prog.data labels data_begin;
  get_label_text prog.text labels text_begin;
  labels

