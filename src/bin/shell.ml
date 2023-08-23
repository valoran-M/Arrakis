let rec run arch =
  Printf.printf "> ";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  match words with
  | command :: _args -> Printf.printf "%s\n" command
  | _ -> run arch
