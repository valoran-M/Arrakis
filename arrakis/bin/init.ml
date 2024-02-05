open Error

let colors_init () =
  if not Options.no_color then Colorsh.setup_std ()

let get_input_file () =
  let file =
    match Options.input_file with
    | []        -> raise (Main_error No_Input_File)
    | hd :: []  -> hd
    | _         -> raise (Main_error Too_Much_Input_File)
  in
  if not (Sys.file_exists file)
  then raise (Main_error (Input_File_Dont_Exist file));
  file

let check_root () =
  match Unix.getuid (), Options.allow_root with
  | 0, true  ->
    Format.eprintf
      "@{<fg_yellow>Warning: Running in root mode. Proceed with caution.@}@."
  | 0, false -> raise (Main_error Running_Root_Without_Opt)
  | _, _     -> ()

let init_syscall () =
  let syscall =
    match Options.env with
    | "unix"  -> Syscall.Scunix.syscall
    | "venus" -> Syscall.Scvenus.syscall
    | s       -> raise (Main_error (Invalid_env s))
  in
  if not Options.unix_socket
  then Syscall.Utils.set_stdout Unix.stdin Unix.stdout Unix.stderr;
  syscall
  

