(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Arch
open Disassembler
open Format
open Gutils.Integer
open Gutils.Print
open Error

exception Break

let ( + ) = Int32.add
let ( * ) = Int32.mul

(* Program -----------------------------------------------------------------  *)

let print_program_header channel =
  fprintf channel "%3s | %2s%2s @{<bold>%-10s@} | @{<bold>Code@}\n"
    "" "" "" "Address"
let print_addr (state : Types.state) addr pc code =
    let breakpoint_str  =
      try  sprintf "%02d" (Hashtbl.find state.breakpoints addr)
      with Not_found -> ""
    in
    let addr_pc   = if addr = pc then ">" else ""              in
    let addr_str  = sprintf "0x%08lx" addr                     in
    let code_str  = print_code state.arch code                 in
    let linenb, _ = Assembler.Debug.get_line state.debug addr  in

    fprintf state.out_channel "%03d | %2s%2s %10s | %s\n"
      linenb
      breakpoint_str
      addr_pc
      addr_str
      code_str

let info_code (state : Types.state) iaddr offset noffset =
  let pc = Arch.Cpu.get_pc state.arch.cpu in
  try
    print_program_header state.out_channel;
    for i=(-noffset) to offset-1 do
      let addr = (iaddr + Int32.of_int i * 0x4l) in
      if addr >= 0l then
        let code = Memory.get_int32 state.arch.memory addr in
        if code = 0l
        then (fprintf state.out_channel "     End without syscall@."; raise Break)
        else print_addr state addr pc code
    done
  with Break -> ()

let execute_info_code args (state : Types.state) =
  let pc = Arch.Cpu.get_pc state.arch.cpu in
  (try
    match args with
    | [] -> info_code state pc 10 4
    | ladr :: l ->
        let adr = Utils.arg_to_int32 state ladr in
        match l with
        | [] -> info_code state adr (Utils.get_size state.labels ladr 10 / 4) 0
        | off :: l ->
            let offset = int_of_string off in
            match l with
            | []        -> info_code state adr offset 0
            | noff :: _ -> info_code state adr offset (int_of_string noff)
  with _ -> raise (Shell_error Bad_Usage));
  state

let info_code : Types.cmd =
  { long_form   = "code";
    short_form  = "c";
    name        = "(c)ode";
    short_desc  = "Print code";
    long_desc   = ["Usage: info code <address> <offset> <negative offset>"];
    execute     = execute_info_code;
    sub         = []; }

(* Memory ------------------------------------------------------------------- *)

let start_default = Segment.data_begin
let size_default  = 0x10

let line_size   = 0x4
let line_size32 = 0x4l

let print_line (state : Types.state) line_address =
  fprintf state.out_channel "0x%08x |" (int32_to_int line_address);
  for i = 0 to line_size - 1 do
    let addr  = line_address + (Int32.of_int i) in
    let value = Memory.get_byte state.arch.memory addr in
    fprintf state.out_channel "  %02lx" value
  done;
  fprintf state.out_channel " \n"

let print_memory (state : Types.state) start size =
  fprintf state.out_channel "@{<bold>%-10s@} |  @{<bold>+0  +1  +2  +3@}\n"
    "Address";
  let line_address = ref (Int32.logand start (Int32.lognot line_size32)) in
  for _ = 1 to size do
    print_line state (alignment !line_address);
    line_address := !line_address + line_size32
  done

let execute_info_memory args (state : Types.state) =
  begin try match args with
  | []      -> print_memory state start_default size_default
  | [start] ->
      let start = Utils.arg_to_int32 state start in
      print_memory state start size_default
  | [start; size] ->
      let start = Utils.arg_to_int32 state start in
      let size  = int_of_string size in
      print_memory state start size
  | [start; size; nsize] ->
      let start = Utils.arg_to_int32 state start in
      let nsize = Int32.of_string nsize in
      let size  = int_of_string   size  in
      print_memory state (Int32.sub start nsize) size
  | _ -> raise (Shell_error Bad_Usage)
  with _ -> raise (Shell_error Bad_Usage)
  end;
  state

let info_memory : Types.cmd =
  { long_form   = "memory";
    short_form  = "m";
    name        = "(m)emory";
    short_desc  = "Print memory segment";
    long_desc   = [
              "Usage: info memory <start> <size> <negative size>";
              "<start>   Either an address or a register containing an address";
      sprintf "<size=%d> Size of the chunk of memory to print (forward)" size_default;
              "<nsize=0> Size of the chunk of memory to print (backward)";
    ];
    execute     = execute_info_memory;
    sub         = []; }

(* Regs --------------------------------------------------------------------- *)

let regs = [|
  "zero (x0)"; "ra   (x1)"; "sp   (x2)"; "gp   (x3)";
  "tp   (x4)"; "t0   (x5)"; "t0   (x6)"; "t1   (x7)";
  "s0   (x8)"; "s1   (x9)"; "a0  (x10)"; "a1  (x11)";
  "a2  (x12)"; "a3  (x13)"; "a4  (x14)"; "a5  (x15)";
  "a6  (x16)"; "a7  (x17)"; "s2  (x18)"; "s3  (x19)";
  "s4  (x20)"; "s5  (x21)"; "s6  (x22)"; "s7  (x23)";
  "s8  (x24)"; "s9  (x25)"; "s10 (x26)"; "s11 (x27)";
  "t3  (x28)"; "t4  (x29)"; "t5  (x30)"; "t6  (x31)";
|]

let print_reg_header (state : Types.state) =
  fprintf state.out_channel "@{<bold>%-9s@} | @{<bold>%-10s@}\n" "Name" "Value"

let info_all_regs (state : Types.state) =
  print_reg_header state;
  for i = 0 to 31 do
    fprintf state.out_channel "%s | 0x%08x\n" regs.(i)
      (int32_to_int (Cpu.get_reg state.arch.cpu i))
  done

let info_list_regs (state : Types.state) =
  print_reg_header state;
  List.iter (fun reg ->
    try
      if reg = "pc" then
        fprintf state.out_channel "pc | 0x%08x\n"
          (int32_to_int (Cpu.get_pc state.arch.cpu))
      else
        let i =
          try Int32.to_int (Assembler.Regs.of_string reg)
          with _ -> int_of_string reg
        in
        fprintf state.out_channel "%s | 0x%08x\n"
          regs.(i) (int32_to_int (Cpu.get_reg state.arch.cpu i))
    with _ ->
      fprintf state.out_channel "%a @{<fg_yellow>'%s'@} isn't a register@." error () reg
  )

let execute_info_registers args (state : Types.state) =
  begin match args with
  | [] -> info_all_regs  state
  | _  -> info_list_regs state args
  end;
  state

let info_registers : Types.cmd =
  { long_form   = "registers";
    short_form  = "r";
    name        = "(r)egisters";
    short_desc  = "Display value in specified registers";
    long_desc   = ["Usage: info registers <r_1> ... <r_n>"];
    execute     = execute_info_registers;
    sub         = []; }

(* Decode ------------------------------------------------------------------- *)

let rec info : Types.cmd =
  { long_form   = "info";
    short_form  = "i";
    name        = "(i)nfo";
    short_desc  = "Print informations about CPU";
    long_desc   = [];
    execute     = (fun _ state -> Help.command info state);
    sub         = [info_memory; info_registers; info_code]; }

