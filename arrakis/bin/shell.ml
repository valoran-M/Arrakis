open Simulator.Arch

let breakpoints = Hashtbl.create 16

let program_run = ref false
let program_end = ref false

let rec run first channel arch =
  if !program_end then
    Format.fprintf channel "@{<fg_red>Error:@} Program is finished@."
  else (
    let addr = Simulator.Cpu.get_pc arch.cpu in
    if first || not (Hashtbl.mem breakpoints addr) then
      match exec_instruction arch with
      | Continue _ -> run false channel arch
      | Zero       ->
        Format.fprintf channel
          "@{<fg_yellow>Warning:@} Syscall is not finished.@.";
        program_end := true;
        program_run := false
      | Sys_call        ->
        match Syscall.syscall channel arch with
        | Syscall.Continue  -> run false channel arch
        | Syscall.Exit code ->
            Format.fprintf channel
              "Exiting with code @{<fg_yellow>'%d'@}@." code;
            program_run := false;
            program_end := true)

let step channel arch =
  if not !program_run && !program_end
  then
    (* TODO: Clarify what this message mean. *)
    Format.fprintf channel "The program is not being run.@."
  else
    match exec_instruction arch with
    | Continue _  -> ()
    | Zero        ->
      Format.fprintf channel
        "@{<fg_yellow>Warning:@} Syscall is not finished.@.";
      program_run := false
    | Sys_call    ->
      match Syscall.syscall channel arch with
      | Syscall.Continue  -> ()
      | Syscall.Exit code ->
        Format.fprintf channel
          "Exiting with code @{<fg_yellow>'%d'@}@." code;
            program_run := false;
            program_end := true

(* Breakpoints -------------------------------------------------------------- *)

let line_breakpoint channel line_debug arg =
  match int_of_string_opt arg with
  | None      -> Format.fprintf channel
                          "@{<fg_red>Error:@} \"%s\" is not a number" arg
  | Some line ->
    try
      let number = Hashtbl.length breakpoints in
      let addr   = Hashtbl.find line_debug line in
      Hashtbl.add breakpoints addr number;
      Format.fprintf channel "Breakpoint %d at 0x%x@." number
        (Int32.to_int addr)
    with Not_found ->
      Format.fprintf channel "@{<fg_red>Error:@} %d line does not exist@." line

let addr_breakpoint channel label arg =
  let number = Hashtbl.length breakpoints in
  match Int32.of_string_opt arg with
  | Some addr ->
    Hashtbl.add breakpoints addr number;
    Format.fprintf channel "Breakpoint %d at 0x%x@." number
      (Int32.to_int addr)
  | None ->
    try
      let addr = Hashtbl.find label arg in
      Hashtbl.add breakpoints addr number;
      Format.fprintf channel "Breakpoint %d at 0x%x@." number
        (Int32.to_int addr)
    with Not_found ->
      Format.fprintf channel
        "@{<fg_red>Error:@} Function \"%s\" not defined.@." arg

exception End_loop

let remove_breakpoint channel arg =
  try
    let breakpoint = int_of_string arg in
    Hashtbl.iter (fun addr line ->
      if line = breakpoint then (
        Hashtbl.remove breakpoints addr;
        Format.fprintf channel "Breakpoint %d was removed@." breakpoint;
        raise End_loop
    )) breakpoints
  with
    | End_loop -> ()
    | _ -> Format.fprintf channel
              "@{<fg_red>Error:@} breakpoint \"%s\" does not exist" arg

let set_breakpoint channel args label line_debug =
  match args with
  | "line"   :: args
  | "l"      :: args -> List.iter (line_breakpoint channel line_debug) args
  | "addr"   :: args
  | "a"      :: args -> List.iter (addr_breakpoint channel label) args
  | "remove" :: args
  | "r"      :: args -> List.iter (remove_breakpoint channel) args
  | "p"      :: _ | "print"  :: _    ->
    Hashtbl.iter (fun addr number ->
      Format.fprintf channel "%3d -> 0x%08x@."
        number (Simulator.Utils.int32_to_int addr)) breakpoints
  | _ -> Help.breakpoint channel

(* Shell -------------------------------------------------------------------- *)

exception Shell_exit

let parse_command channel arch command args label addr_debug line_debug =
  match command with
  | "run"         | "r" ->
    program_run := true;
    run false channel arch;
  | "breakpoint"  | "b" -> set_breakpoint channel args label line_debug
  | "step"        | "s" -> step channel arch
  | "next"        | "n" -> run true channel arch
  | "print"       | "p" -> Print.decode_print channel arch args addr_debug breakpoints
  | "help"        | "h" -> Help.general channel
  | "quit"        | "q" -> raise Shell_exit
  | _ ->
      Format.fprintf channel "@{<fg_red>Error:@} Undefined command: \
        @{<fg_yellow>\"%s\"@}. Try @{<fg_green>\"help\"@}.@."command

let rec shell arch label addr_debug line_debug =
  if !program_run && not !program_end then
    Print.print_code_part
    (Format.formatter_of_out_channel stdout) arch addr_debug breakpoints 8 0;
  Format.printf "> %!";
  let line = read_line () in
  let words = String.split_on_char ' ' line in
  try match words with
  | command :: args ->
    parse_command Format.std_formatter arch
        command args label addr_debug line_debug;
    shell arch label addr_debug line_debug
  | _ -> shell arch label addr_debug line_debug
  with Shell_exit -> ()

