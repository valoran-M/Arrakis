(* colorsh ********************************************************************)
(* Copyright 2023-2024 Gurvan Debaussart (https://debauss.art)                *)
(* Distributed under the MIT license                                          *)
(******************************************************************************)

open Format

let open_tag = function
    | String_tag s ->
        begin match s with
        (* Foreground colors *)
        | "fg_black"    -> "\x1b[30m"
        | "fg_red"      -> "\x1b[31m"
        | "fg_green"    -> "\x1b[32m"
        | "fg_yellow"   -> "\x1b[33m"
        | "fg_blue"     -> "\x1b[34m"
        | "fg_purple"   -> "\x1b[35m"
        | "fg_cyan"     -> "\x1b[36m"
        | "fg_white"    -> "\x1b[37m"
        (* Background color *)
        | "bg_black"    -> "\x1b[40m"
        | "bg_red"      -> "\x1b[41m"
        | "bg_green"    -> "\x1b[42m"
        | "bg_yellow"   -> "\x1b[43m"
        | "bg_blue"     -> "\x1b[44m"
        | "bg_purple"   -> "\x1b[45m"
        | "bg_cyan"     -> "\x1b[46m"
        | "bg_white"    -> "\x1b[47m"
        (* Text style *)
        | "bold"        -> "\x1b[1m"
        | "dim"         -> "\x1b[2m"
        | "italic"      -> "\x1b[3m"
        | "underline"   -> "\x1b[4m"
        | "reversed"    -> "\x1b[7m"
        | "strike"      -> "\x1b[9m"
        (* Unrecognized *)
        | _ -> ""
        end
    | _ -> ""

let close_tag = function
    | String_tag s ->
        begin match s with
        (* Foreground colors *)
        | "fg_black"
        | "fg_red"
        | "fg_green"
        | "fg_yellow"
        | "fg_blue"
        | "fg_purple"
        | "fg_cyan"
        | "fg_white"    -> "\x1b[39m"
        (* Background color *)
        | "bg_black"
        | "bg_red"
        | "bg_green"
        | "bg_yellow"
        | "bg_blue"
        | "bg_purple"
        | "bg_cyan"
        | "bg_white"    -> "\x1b[49m"
        (* Text style *)
        | "bold"        -> "\x1b[22m"
        | "dim"         -> "\x1b[22m"
        | "italic"      -> "\x1b[23m"
        | "underline"   -> "\x1b[24m"
        | "reversed"    -> "\x1b[27m"
        | "strike"      -> "\x1b[29m"
        (* Unrecognized *)
        | _ -> ""
        end
    | _ -> ""

let setup_fmt fmt =
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

