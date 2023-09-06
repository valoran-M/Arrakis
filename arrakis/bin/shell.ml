open Simulator.Arch

let breakpoints = Hashtbl.create 16

let program_run = ref false

let rec run chanel arch =
  let addr = Simulator.Cpu.get_pc arch.cpu in
  if not (Hashtbl.mem breakpoints addr) then
  match exec_instruction arch with
  | Continue _addr  -> run chanel arch
  | Zero            ->
    Printf.fprintf chanel "Warning: not syscal end\n%!";
    program_run := false
  | Sys_call        -> failwith "TODO"

let step chanel arch =
  if not !program_run
  then Printf.fprintf chanel "The program is not being run.\n%!"
  else
  match exec_instruction arch with
  | Continue _  -> ()
  | Zero        ->
    Printf.fprintf chanel "Warning: not syscal end\n%!";
    program_run := false
  | Sys_call    -> failwith "TODO"

(* Breakpoints -------------------------------------------------------------- *)

let help_breakpoint chanel =
  Printf.fprintf chanel {|
Breakpoint help :

(b)reakpoint (l)ine   l1 [l2 ...] -> Set breakpoints on line l1 ...
(b)reakpoint (a)ddr   a1 [a2 ...] -> Set breakpoints on addresses a1 ...
(b)reakpoint (r)emove b1 [b2 ...] -> Remove breakpoints b1 ...
(b)reakpoint (p)rint              -> Print all breakpoints
%!|}

let line_breakpoint chanel line_debug arg =
  match int_of_string_opt arg with
  | None      -> Printf.fprintf chanel "Error : \"%s\" is not a number" arg
  | Some line ->
    try
      let number = Hashtbl.length breakpoints in
      let addr   = Hashtbl.find line_debug line in
      Hashtbl.add breakpoints addr number;
      Printf.fprintf chanel "Breakpoint %d at 0x%x\n%!" number
        (Int32.to_int addr)
    with Not_found ->
      Printf.fprintf chanel "Error : %d line does not exist\n%!" line

let addr_breakpoint chanel label arg =
  let number = Hashtbl.length breakpoints in
  match Int32.of_string_opt arg with
  | Some addr ->
    Hashtbl.add breakpoints addr number;
    Printf.fprintf chanel "Breakpoint %d at 0x%x\n%!" number
      (Int32.to_int addr)
  | None ->
    try
      let addr = Hashtbl.find label arg in
      Hashtbl.add breakpoints addr number;
      Printf.fprintf chanel "Breakpoint %d at 0x%x\n%!" number
        (Int32.to_int addr)
    with Not_found ->
      Printf.fprintf chanel "Function \"%s\" not defined.\n%!" arg

exception End_loop

let remove_breakpoint chanel arg =
  try
    let breakpoint = int_of_string arg in
    Hashtbl.iter (fun addr line -> 
      if line = breakpoint then (
        Hashtbl.remove breakpoints addr;
        Printf.fprintf chanel "Breakpoint %d was removed\n%!" breakpoint;
        raise End_loop
    )) breakpoints
  with 
    | End_loop -> ()
    | _ -> Printf.fprintf chanel "Error : breakpoint \"%s\" does not exist" arg

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
      Printf.fprintf chanel "%3d -> 0x%08x\n%!"
        number (Simulator.Utils.int32_to_int addr)) breakpoints
  | _ -> help_breakpoint chanel

(* Shell -------------------------------------------------------------------- *)

let print_help chanel =
  Printf.fprintf chanel {|
Commands :

(r)un -> run code

(b)reakpoint -> create breakpoint
(s)tep       -> In debug mode execute next instruction
(n)ext       -> run to next breakpoint
(p)rint      -> print args

(h)elp -> show this help
(q)uit
%!|}

exception Shell_exit

let parse_command chanel arch command args label addr_debug line_debug =
  match command with
  | "run"         | "r" ->
    program_run := true;
    run chanel arch;
  | "breakpoint"  | "b" -> set_breakpoint chanel args label line_debug
  | "step"        | "s" -> step chanel arch
  | "next"        | "n" -> Printf.fprintf chanel "Unimplemented for now.\n"
  | "print"       | "p" -> Print.decode_print arch args addr_debug
  | "help"        | "h" -> print_help chanel
  | "quit"        | "q" -> raise Shell_exit
  | _ ->
    Printf.fprintf chanel "Undefined command: \"%s\".  Try \"help\".\n%!"command

let rec shell arch label addr_debug line_debug =
  Printf.printf "> ";
  if !program_run then Print.print_prog arch 8 addr_debug;
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    parse_command stdout arch command args label addr_debug line_debug;
    shell arch label addr_debug line_debug
  | _ -> shell arch label addr_debug line_debug
  with Shell_exit -> ()

