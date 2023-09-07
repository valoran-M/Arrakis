open Simulator.Arch

let breakpoints = Hashtbl.create 16

let program_run = ref false

let rec run chanel arch =
  let addr = Simulator.Cpu.get_pc arch.cpu in
  if not (Hashtbl.mem breakpoints addr) then
  match exec_instruction arch with
  | Continue _addr  -> run chanel arch
  | Zero            ->
    Format.fprintf chanel "@{<fg_yellow>Warning:@} not syscal end@.";
    program_run := false
  | Sys_call        -> failwith "TODO"

let step chanel arch =
  if not !program_run
  then Format.fprintf chanel "The program is not being run.@."
  else
  match exec_instruction arch with
  | Continue _  -> ()
  | Zero        ->
    Format.fprintf chanel "@{<fg_yellow>Warning:@} not syscal end@.";
    program_run := false
  | Sys_call    -> failwith "TODO"

(* Breakpoints -------------------------------------------------------------- *)

let help_breakpoint chanel =
  Format.fprintf chanel {|
Breakpoint help :

(b)reakpoint (l)ine   l1 [l2 ...] -> Set breakpoints on line l1 ...
(b)reakpoint (a)ddr   a1 [a2 ...] -> Set breakpoints on addresses a1 ...
(b)reakpoint (r)emove b1 [b2 ...] -> Remove breakpoints b1 ...
(b)reakpoint (p)rint              -> Print all breakpoints
%!|}

let line_breakpoint chanel line_debug arg =
  match int_of_string_opt arg with
  | None      -> Format.fprintf chanel "@{fg_red>Error:@} \"%s\" is not a number" arg
  | Some line ->
    try
      let number = Hashtbl.length breakpoints in
      let addr   = Hashtbl.find line_debug line in
      Hashtbl.add breakpoints addr number;
      Format.fprintf chanel "Breakpoint %d at 0x%x@." number
        (Int32.to_int addr)
    with Not_found ->
      Format.fprintf chanel "@{<fg_red>Error:@} %d line does not exist@." line

let addr_breakpoint chanel label arg =
  let number = Hashtbl.length breakpoints in
  match Int32.of_string_opt arg with
  | Some addr ->
    Hashtbl.add breakpoints addr number;
    Format.fprintf chanel "Breakpoint %d at 0x%x@." number
      (Int32.to_int addr)
  | None ->
    try
      let addr = Hashtbl.find label arg in
      Hashtbl.add breakpoints addr number;
      Format.fprintf chanel "Breakpoint %d at 0x%x@." number
        (Int32.to_int addr)
    with Not_found ->
      Format.fprintf chanel "@{<fg_red>Error:@} Function \"%s\" not defined.@." arg

exception End_loop

let remove_breakpoint chanel arg =
  try
    let breakpoint = int_of_string arg in
    Hashtbl.iter (fun addr line ->
      if line = breakpoint then (
        Hashtbl.remove breakpoints addr;
        Format.fprintf chanel "Breakpoint %d was removed@." breakpoint;
        raise End_loop
    )) breakpoints
  with
    | End_loop -> ()
    | _ -> Format.fprintf chanel "@{<fg_red>Error:@} breakpoint \"%s\" does not exist" arg

let set_breakpoint chanel args label line_debug =
  match args with
  | "line" :: args
  | "l"    :: args -> List.iter (line_breakpoint chanel line_debug) args
  | "addr" :: args
  | "a"    :: args -> List.iter (addr_breakpoint chanel label) args
  | "remove" :: args
  | "r"      :: args -> List.iter (remove_breakpoint chanel) args
  | "p"     :: _
  | "print" :: _ ->
    Hashtbl.iter (fun addr number ->
      Format.fprintf chanel "%3d -> 0x%08x@."
        number (Simulator.Utils.int32_to_int addr)) breakpoints
  | _ -> help_breakpoint chanel

(* Shell -------------------------------------------------------------------- *)

let print_help chanel =
  Format.fprintf chanel {|
Commands :

(r)un -> run code

(b)reakpoint -> create breakpoint
(s)tep       -> In debug mode execute next instruction
(n)ext       -> run to next breakpoint
(p)rint      -> print args

(h)elp -> show this help
(q)uit
@.|}

exception Shell_exit

let parse_command chanel arch command args label addr_debug line_debug =
  match command with
  | "run"         | "r" ->
    program_run := true;
    run chanel arch;
  | "breakpoint"  | "b" -> set_breakpoint chanel args label line_debug
  | "step"        | "s" -> step chanel arch
  | "next"        | "n" -> Format.fprintf chanel "@{<fg_yellow>Unimplemented for now.@}@."
  | "print"       | "p" -> Print.decode_print arch args addr_debug
  | "help"        | "h" -> print_help chanel
  | "quit"        | "q" -> raise Shell_exit
  | _ ->
      Format.fprintf chanel "@{<fg_red>Error:@} Undefined command: \"%s\". Try \"help\".@."command

let rec shell arch label addr_debug line_debug =
  Format.printf "> %!";
  if !program_run then Print.print_prog arch 8 addr_debug;
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    parse_command Format.std_formatter arch command args label addr_debug line_debug;
    shell arch label addr_debug line_debug
  | _ -> shell arch label addr_debug line_debug
  with Shell_exit -> ()

