open Simulator.Arch

let rec run arch =
  match exec_instruction arch with
  | Continue -> run arch
  | Zero -> Printf.printf "Waring: not syscal end\n"
  | Sys_call -> failwith "TODO"

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

let parse_command arch command _args =
  match command with
  | "run" | "r"  -> run arch
  | "help" -> print_help ()
  | _ -> Printf.printf "Undefined command: \"%s\".  Try \"help\".\n" command

let rec shell arch =
  Printf.printf "> ";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  match words with
  | "exit" :: _ | "e" :: _ -> ()
  | command :: args -> parse_command arch command args; shell arch
  | _ -> shell arch

