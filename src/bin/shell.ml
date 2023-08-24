open Simulator.Arch

let rec run arch =
  match exec_instruction arch with
  | Continue -> run arch
  | Zero -> Printf.printf "Waring: not syscal end\n"
  | Sys_call -> failwith "TODO"

let parse_command arch command _args =
  match command with
  | "run" -> run arch
  | _ -> Printf.printf "%s\n" command

let rec shell arch =
  Printf.printf "> ";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  match words with
  | command :: args -> parse_command arch command args
  | _ -> shell arch

