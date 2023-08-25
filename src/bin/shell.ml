open Simulator.Arch

let breakpoints = Hashtbl.create 16

let program_run = ref false

let rec run arch =
  let addr = Simulator.Cpu.get_pc arch.cpu in
  if not (Hashtbl.mem breakpoints addr) then
  match exec_instruction arch with
  | Continue _addr  -> run arch
  | Zero            ->
    Printf.printf "Warning: not syscal end\n";
    program_run := false
  | Sys_call        -> failwith "TODO"

let step arch =
  match exec_instruction arch with
  | Continue _  -> ()
  | Zero        -> Printf.printf "Warning: not syscal end\n"
  | Sys_call    -> failwith "TODO"

let set_breakpoint args label =
  try
    List.iter (fun arg ->
      let number = Hashtbl.length breakpoints in
      try
        let addr = Int32.of_string arg in
        Hashtbl.add breakpoints addr number;
        Printf.printf "Breakpoint %d at 0x%x\n" number (Int32.to_int addr)
      with Failure _ ->
      try
        let addr = Hashtbl.find label arg in
        Hashtbl.add breakpoints addr number;
        Printf.printf "Breakpoint %d at 0x%x\n" number (Int32.to_int addr)
      with Not_found -> Printf.printf "Function \"%s\" not defined.\n" arg
    ) args
  with _ -> ()

let print_help () =
  print_string {|
Commands :

(r)un -> run code

(b)reakpoint -> create breakpoint
(s)tep       -> In debug mode execute next instruction
(n)ext       -> run to next breakpoint
(p)rint      -> print args

(h)elp -> show this help
(q)uit
|}

exception Shell_exit

let parse_command arch command args label =
  match command with
  | "run"         | "r" -> program_run := true; run  arch
  | "breakpoint"  | "b" -> set_breakpoint args label
  | "step"        | "s" -> step arch
  | "next"        | "n" -> Printf.printf "Unimplemented for now.\n"
  | "print"       | "p" -> Printf.printf "Unimplemented for now.\n"
  | "help"        | "h" -> print_help ()
  | "quit"        | "q" -> raise Shell_exit
  | _ -> Printf.printf "Undefined command: \"%s\".  Try \"help\".\n" command

let rec shell arch label debug =
  if !program_run then Show.print_prog arch debug;
  Printf.printf "> ";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    parse_command arch command args label;
    shell arch label debug
  | _ -> shell arch label debug
  with Shell_exit -> ()

