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

(* breakpoints -------------------------------------------------------------- *)

let help_breakpoint chanel =
  Printf.fprintf chanel {|
Breakpoint help :

(b)reakpoint (l)ine l1 [l2 ...] -> Set breakpoints on line l1 ...

(b)reakpoint (a)ddr a1 [a2 ...] -> Set breakpoints on addresses a1 ...
%!|}

let line_breakpoint chanel args line_debug =
  List.iter (fun arg ->
    match int_of_string_opt arg with
    | None      -> Printf.fprintf chanel "Error : \"%s\" is not a number" arg
    | Some line ->
      try
        let number = Hashtbl.length breakpoints in
        let addr   = Hashtbl.find line_debug line in
        Hashtbl.add breakpoints addr number
      with Not_found ->
        Printf.fprintf chanel "Error : %d line does not exist\n%!" line
  ) args

let addr_breakpoint chanel args label =
  List.iter (fun arg ->
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
  ) args

let set_breakpoint chanel args label line_debug =
  match args with
  | "line" :: args -> line_breakpoint chanel args line_debug
  | "addr" :: args -> addr_breakpoint chanel args label
  | "remove" :: _args -> failwith "TODO"
  | _ -> help_breakpoint chanel

(* shell -------------------------------------------------------------------- *)

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

let parse_command chanel arch command args label line_debug =
  match command with
  | "run"         | "r" ->
    program_run := true;
    run chanel arch;
  | "breakpoint"  | "b" -> set_breakpoint chanel args label line_debug
  | "step"        | "s" -> step chanel arch
  | "next"        | "n" -> Printf.fprintf chanel "Unimplemented for now.\n"
  | "print"       | "p" -> Print.decode_print arch args
  | "help"        | "h" -> print_help chanel
  | "quit"        | "q" -> raise Shell_exit
  | _ ->
    Printf.fprintf chanel "Undefined command: \"%s\".  Try \"help\".\n%!"command

let rec shell arch label addr_debug line_debug =
  Printf.printf "> ";
  if !program_run then Print.print_prog arch addr_debug;
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    parse_command stdout arch command args label line_debug;
    shell arch label addr_debug line_debug
  | _ -> shell arch label addr_debug line_debug
  with Shell_exit -> ()

