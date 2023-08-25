open Simulator.Arch

let breakpoints = Hashtbl.create 16

let program_run = ref false

let rec run arch =
  let addr = Simulator.Cpu.get_pc arch.cpu in
  if not (Hashtbl.mem breakpoints addr) then
  match exec_instruction arch with
  | Continue _addr ->  run arch
  | Zero -> Printf.printf "Waring: not syscal end\n"
  | Sys_call -> failwith "TODO"

let step arch =
  match exec_instruction arch with
  | Continue _ -> ()
  | Zero -> Printf.printf "Waring: not syscal end\n"
  | Sys_call -> failwith "TODO"

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
Commandes :

(r)un -> run code

(b)reakpoint -> create breakpoint
(s)tep -> In debug mode execute next instruction
(n)ext -> run to next breakpoint
(sh)ow -> show args

(e)xit
|}

let parse_command arch command args label =
  match command with
  | "run"         | "r" -> program_run := true; run  arch
  | "step"        | "s" -> step arch
  | "breakpoints" | "b" -> set_breakpoint args label
  | "help" -> print_help ()
  | _ -> Printf.printf "Undefined command: \"%s\".  Try \"help\".\n" command

let rec shell arch label =
  if !program_run then Show.print_prog arch;
  Printf.printf "> ";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  match words with
  | "exit" :: _ | "e" :: _ -> ()
  | command :: args ->
    parse_command arch command args label;
    shell arch label
  | _ -> shell arch label

