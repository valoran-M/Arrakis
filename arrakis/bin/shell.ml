open Simulator.Execute

let breakpoints = Hashtbl.create 16

let program_run = ref false
let program_end = ref false

let step channel arch history syscall =
  let open Syscall.Types in
  if !program_end
  then (
    Format.fprintf channel
        "\n@{<fg_red>Error:@} Program has exited, can't run further.@.";
    history
  ) else
    match exec_instruction arch history with
    | Continue (_, history)  -> history
    | Zero        ->
      Format.fprintf channel
        "\n@{<fg_yellow>Warning:@} Exiting without an exit syscall.@.";
      program_end := true; history
    | Sys_call history  ->
      match syscall channel arch with
      | Continue  -> history
      | Exit code ->
        Format.fprintf channel
          "\n@{<fg_blue>Info:@} Exiting with code @{<fg_yellow>'%d'@}.@."
          code;
        program_end := true;
        history

let rec run first channel (arch : Simulator.Arch.t) history syscall =
  if !program_end then history else
  let addr = Simulator.Cpu.get_pc arch.cpu in
  if first || not (Hashtbl.mem breakpoints addr) then (
      Printf.printf "OK\n%!";
      let history = step channel arch history syscall in
      if !program_end
      then history
      else run false channel arch history syscall
  ) else history

let prev channel arch history =
  try Simulator.History.step_back arch history
  with Simulator.History.History_Empty ->
    Format.fprintf channel
  "\n@{<fg_red>Error:@} History is empty.@."; history

let reset arch history = Simulator.History.reset arch history

(* Breakpoints -------------------------------------------------------------- *)

let line_breakpoint channel line_debug arg =
  match int_of_string_opt arg with
  | None      -> Format.fprintf channel
                  "@{<fg_red>Error:@} \"%s\" is not a number.@." arg
  | Some line ->
    try
      let number = Hashtbl.length breakpoints   in
      let addr   = Hashtbl.find line_debug line in
      Hashtbl.add breakpoints addr number;
      Format.fprintf channel
        "@{<fg_blue>Info:@} Created breakpoint %d at 0x%x@."
        number (Int32.to_int addr)
    with Not_found ->
      Format.fprintf channel
        "@{<fg_red>Error:@} Line %d does not contain code.@." line

let addr_breakpoint channel label arg =
  let number = Hashtbl.length breakpoints in
  match Int32.of_string_opt arg with
  | Some addr ->
    Hashtbl.add breakpoints addr number;
    Format.fprintf channel
      "@{<fg_blue>Info:@} Created breakpoint %d at 0x%x.@."
      number (Int32.to_int addr)
  | None ->
    try
      let addr = Hashtbl.find label arg in
      Hashtbl.add breakpoints addr number;
      Format.fprintf channel
        "@{<fg_blue>Info:@} Created breakpoint %d at 0x%x.@."
        number (Int32.to_int addr)
    with Not_found ->
      Format.fprintf channel
        "@{<fg_red>Error:@} Function \"%s\" not defined.@." arg

exception End_loop

let remove_breakpoint channel arg =
  try
    let breakpoint = int_of_string arg in
    Hashtbl.iter (fun addr line ->
      if line = breakpoint then (
        Hashtbl.remove breakpoints addr;
        Format.fprintf channel
          "@{<fg_blue>Info:@}Breakpoint %d was removed.@." breakpoint;
        raise End_loop
    )) breakpoints
  with
    | End_loop -> ()
    | _        ->
      Format.fprintf channel
        "@{<fg_red>Error:@} Breakpoint \"%s\" does not exist.@." arg

let iter channel f l =
  if List.length l == 0 then
      Format.fprintf channel
        "@{<fg_red>Error:@} Command require at least one argument.@."
  else List.iter f l

let set_breakpoint channel args label line_debug =
  match args with
  | "line"   :: args
  | "l"      :: args -> iter channel (line_breakpoint channel line_debug) args
  | "addr"   :: args
  | "a"      :: args -> iter channel (addr_breakpoint channel label) args
  | "remove" :: args
  | "r"      :: args -> iter channel (remove_breakpoint channel) args
  | "print"  :: _
  | "p"      :: _    ->
      Hashtbl.iter (fun addr number ->
        Format.fprintf channel "%3d -> 0x%08x@."
        number (Simulator.Utils.int32_to_int addr)) breakpoints
  | _ -> Help.breakpoint channel

(* Shell -------------------------------------------------------------------- *)

exception Shell_exit

let parse_command channel arch history command args
                  label addr_debug line_debug syscall =
  match command with
  | "run"         | "r" ->
      program_run := true;
      run false channel arch history syscall;
  | "breakpoint"  | "b"  -> set_breakpoint channel args label line_debug; history
  | "step"        | "s"  -> step channel arch history syscall
  | "next"        | "n"  -> run true channel arch history syscall
  | "help"        | "h"  -> Help.general channel; history
  | "quit"        | "q"  -> raise Shell_exit
  | "prev"        | "prv"-> prev channel arch history
  | "reset"       | "res"-> reset arch history
  | "print"       | "p"  ->
    Print.decode_print channel arch args addr_debug breakpoints; history
  | _ ->
      Format.fprintf channel
      "@{<fg_red>Error:@} Undefined command: @{<fg_yellow>\"%s\"@}. \
      Try @{<fg_green>\"help\"@}.@." command; history

let rec shell arch history label addr_debug line_debug syscall =
  if !program_run && not !program_end then
    Print.print_code_part
    (Format.formatter_of_out_channel stdout) arch addr_debug breakpoints 8 0;
  Format.printf "> %!";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    let history = parse_command Format.std_formatter arch history
        command args label addr_debug line_debug syscall in
    shell arch history label addr_debug line_debug syscall
  | _ -> shell arch history label addr_debug line_debug syscall
  with Shell_exit -> ()

