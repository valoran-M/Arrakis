type input =
  | Dir of string
  | File of string

let test_input =
  match Sys.argv |> Array.to_list |> List.tl with
  | [path] ->
    if not (Sys.file_exists path) then (
      Printf.eprintf "%s: not found\n" path;
      exit 1
    );
    if Sys.is_directory path
    then Dir path
    else File path
  | _ ->
    Format.eprintf "usage: dune exec run_tests -- <tests_directory>\n";
    exit 1

let ensure_newline = function
  | "" -> ""
  | s ->
    if s.[String.length s - 1] = '\n' then s
    else s ^ "\n"

let read_all_gen read cin =
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let rec loop () =
    match read cin b 0 4096 with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf b 0 n; loop ()
  in
  loop ();
  Buffer.contents buf

let read_all_channel = read_all_gen input

let run_command command input =
  let stdout, stdin, stderr =
    Unix.open_process_args_full command.(0) command (Unix.environment ())
  in
  output_string stdin input;
  flush stdin;
  let out = read_all_channel stdout in
  let err = read_all_channel stderr in
  let ret = Unix.close_process_full (stdout, stdin, stderr) = WEXITED(0) in
  out, err, ret

let run_file f_in f_out =
  let open String in
  let in_file = open_in f_in in
  let command = input_line in_file |> split_on_char ' ' |> Array.of_list in
  let pinput  = read_all_channel in_file in
  let o, e, s = run_command command pinput in
  let ref_out = open_in f_out |> read_all_channel in
  
  if ref_out = o
  then Ok s
  else Error (o, e, s, ref_out)

let green s = "\027[1;32m" ^ s ^ "\027[0m"
let red s = "\027[1;31m" ^ s ^ "\027[0m"

open Format

let test f_in f_out =
  let file = Filename.basename f_in |> Filename.remove_extension in
  Printf.printf "Testing %s... %!" file;
  match run_file f_in f_out with
  | Ok s ->
    Printf.printf "%s %s\n%!" (green "[OK]") (if s then "" else "(failure)")
  | Error (o, e, _, ro) ->
    printf "%s\n" (red "[ERROR]");
    printf "%s\n" (red "==>");
    printf "- Running program produces :\n";
    printf "* Out:\n%s* Err:\n%s\n" (ensure_newline o) (ensure_newline e);
    printf "- Running the interpreter on the program produces :\n";
    printf "%s\n" (ensure_newline ro)

let test_dir tests_dir =
  let testfiles_in =
    Sys.readdir tests_dir
    |> Array.to_list
    |> List.filter (fun file -> Filename.extension file = ".in")
    |> List.sort (String.compare)
    |> List.map (fun file -> Filename.concat tests_dir file)
  in
  let testfiles_out =
    Sys.readdir tests_dir
    |> Array.to_list
    |> List.filter (fun file -> Filename.extension file = ".out")
    |> List.map (fun file -> Filename.concat tests_dir file)
  in
  List.iter2 (fun f_in f_out -> test f_in f_out) testfiles_in testfiles_out

let test_file f =
  let file = Filename.remove_extension f in
  let f_in  = file ^ ".in" in
  let f_out = file ^ ".out" in
  test f_in f_out

let () =
  match test_input with
  | Dir d  -> test_dir d
  | File f -> test_file f

