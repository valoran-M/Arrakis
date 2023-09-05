open Simulator.Arch

let breakpoints = Hashtbl.create 16

let program_run = ref false

let rec run chanel arch =
  let addr = Simulator.Cpu.get_pc arch.cpu in
  if not (Hashtbl.mem breakpoints addr) then
  match exec_instruction arch with
  | Continue _addr  -> run chanel arch
  | Zero            ->
    Printf.fprintf chanel "Warning: not syscal end%!";
    program_run := false
  | Sys_call        -> failwith "TODO"

let step chanel arch =
  if not !program_run
  then Printf.fprintf chanel "The program is not being run.\n%!"
  else
  match exec_instruction arch with
  | Continue _  -> ()
  | Zero        ->
    Printf.fprintf chanel "Warning: not syscal end%!";
    program_run := false
  | Sys_call    -> failwith "TODO"

let set_breakpoint chanel args label =
  try
    List.iter (fun arg ->
      let number = Hashtbl.length breakpoints in
      try
        let addr = Int32.of_string arg in
        Hashtbl.add breakpoints addr number;
        Printf.fprintf chanel "Breakpoint %d at 0x%x\n%!" number
          (Int32.to_int addr)
      with Failure _ ->
      try
        let addr = Hashtbl.find label arg in
        Hashtbl.add breakpoints addr number;
        Printf.fprintf chanel "Breakpoint %d at 0x%x\n%!" number
          (Int32.to_int addr)
      with Not_found ->
        Printf.fprintf chanel "Function \"%s\" not defined.\n%!" arg
    ) args
  with _ -> ()

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

let parse_command chanel arch command args label debug =
  match command with
  | "run"         | "r" ->
    program_run := true;
    run chanel arch;
  | "breakpoint"  | "b" -> set_breakpoint chanel args label
  | "step"        | "s" -> step chanel arch; Print.print_prog arch debug
  | "next"        | "n" -> Printf.fprintf chanel "Unimplemented for now.\n"
  | "print"       | "p" -> Print.decode_print arch args
  | "help"        | "h" -> print_help chanel
  | "quit"        | "q" -> raise Shell_exit
  | _ ->
    Printf.fprintf chanel "Undefined command: \"%s\".  Try \"help\".\n%!"command

let rec shell arch label debug =
  Printf.printf "> ";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    parse_command stdout arch command args label debug;
    shell arch label debug
  | _ -> shell arch label debug
  with Shell_exit -> ()

