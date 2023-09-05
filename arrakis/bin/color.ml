open Options
open Format

let tag_table = Hashtbl.create 100

(* Foreground Color --------------------------------------------------------- *)

let () =
  List.iter (fun (tag, sint) ->
    let ostr  = sprintf  "[%dm" sint in
    let cstr  = "[37m" in
    Hashtbl.add tag_table tag (ostr, cstr)
  )
  [
    (* Tag    Shell int *)
    "fg_black",     30;
    "fg_red",       31;
    "fg_green",     32;
    "fg_yellow",    33;
    "fg_blue",      34;
    "fg_purple",    35;
    "fg_cyan",      36;
    "fg_white",     37;
  ]

(* Background Color --------------------------------------------------------- *)

(* TODO *)

(* Bold, Underline ---------------------------------------------------------- *)

let () =
  List.iter (fun (tag, oint, cint) ->
    let ostr  = sprintf  "[%dm" oint in
    let cstr  = sprintf  "[%dm" cint in
    Hashtbl.add tag_table tag (ostr, cstr)
  )
  [
    (* Tag   Open int    Close int *)
    "bold",           1, 22;
    "underline",      4, 24;
  ]

(* -------------------------------------------------------------------------- *)

let open_tag = function
  | Format.String_tag s   -> (
        try let ostr, _ = Hashtbl.find tag_table s in ostr
        with Not_found -> ""
      )
  | _ -> ""

let close_tag = function
  | Format.String_tag s   -> (
        try
          let _, otag = Hashtbl.find tag_table s in otag
        with Not_found -> ""
      )
  | _                     -> ""

let activate_color_tag fmt =
  if no_color then () else
  pp_set_tags fmt true;
  let sf = pp_get_formatter_stag_functions fmt () in
  pp_set_formatter_stag_functions fmt
  {
    sf with
    mark_open_stag  = open_tag;
    mark_close_stag = close_tag;
  }

let setup () =
  activate_color_tag err_formatter;
  activate_color_tag std_formatter
