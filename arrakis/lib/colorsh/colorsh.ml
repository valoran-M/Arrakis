(******************************************************************************)
(*  Copyright 2023 Gurvan Debaussart                                          *)
(*  This file is distributed under the MIT license.                           *)
(*  https://codeberg.org/gurvan/colorsh                                       *)
(******************************************************************************)

open Format

let tag_table = Hashtbl.create 18
let inited    = ref false

let init () =
  inited := true;

  (* Foreground Color ------------------------------------------------------- *)

  List.iter (fun (tag, sint) ->
    let ostr = sprintf  "[%dm" sint in
    let cstr =          "[39m"      in
    Hashtbl.add tag_table tag (ostr, cstr)
  )
  [
    (* Tag        Open *)
    "fg_black",   30;
    "fg_red",     31;
    "fg_green",   32;
    "fg_yellow",  33;
    "fg_blue",    34;
    "fg_purple",  35;
    "fg_cyan",    36;
    "fg_white",   37;
  ];

  (* Background Color ------------------------------------------------------- *)

  List.iter (fun (tag, sint) ->
    let ostr = sprintf  "[%dm" sint in
    let cstr =          "[49m"      in
    Hashtbl.add tag_table tag (ostr, cstr)
  )
  [
    (* Tag        Open *)
    "bg_black",   40;
    "bg_red",     41;
    "bg_green",   42;
    "bg_yellow",  43;
    "bg_blue",    44;
    "bg_purple",  45;
    "bg_cyan",    46;
    "bg_white",   47;
  ];

  (* Text style ------------------------------------------------------------- *)

  List.iter (fun (tag, oint, cint) ->
    let ostr = sprintf "[%dm" oint in
    let cstr = sprintf "[%dm" cint in
    Hashtbl.add tag_table tag (ostr, cstr)
  )
  [
    (* Tag       Open   Close  *)
    "bold",      1,     22;
    "dim",       2,     22;
    "italic",    3,     23;
    "underline", 4,     24;
    "reversed",  7,     27;
    "strike",    9,     29;
  ]

(* -------------------------------------------------------------------------- *)

let open_tag = function
  | Format.String_tag s ->
      begin match Hashtbl.find_opt tag_table s with
      | Some (otag, _) -> otag
      | None           -> ""
      end
  | _ -> ""

let close_tag = function
  | Format.String_tag s ->
      begin match Hashtbl.find_opt tag_table s with
      | Some (_, ctag) -> ctag
      | None           -> ""
      end
  | _ -> ""

let setup_fmt fmt =
  (if !inited = false then init ());
  pp_set_tags fmt true;
  let sf = pp_get_formatter_stag_functions fmt () in
  pp_set_formatter_stag_functions fmt
  {
    sf with
    mark_open_stag  = open_tag;
    mark_close_stag = close_tag;
  }

let setup_std () =
  setup_fmt err_formatter;
  setup_fmt std_formatter
