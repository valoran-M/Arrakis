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

(* Progam ------------------------------------------------------------------  *)

let print_program_header channel =
  fprintf channel "%3s | %2s%2s @{<bold>%-10s@} | @{<bold>%-12s@} | @{<bold>%-18s@} | @{<bold>%-18s@} |\n"
    "" "" "" "Address" "Machine Code" "Basic Code" "Original Code"

let print_addr (state : Types.state) addr pc code =
    let breakpoint_str  =
      try  sprintf "%02d" (Hashtbl.find state.breakpoints addr)
      with Not_found -> ""
    in
    let addr_pc               = if addr = pc then ">" else ""              in
    let addr_str              = int32_to_int addr                          in
    let addr_str              = sprintf "0x%08x" addr_str                  in
    let machinec_str          = int32_to_int code                          in
    let machinec_str          = sprintf "0x%08x" machinec_str              in
    let basicc_str            = print_code state.arch code                 in
    let linenb, orignal_code  = Assembler.Debug.get_line state.debug addr  in

    fprintf state.out_channel "%03d | %2s%2s %10s | %12s | %-18s | %-18s |\n"
      linenb
      breakpoint_str
      addr_pc
      addr_str
      machinec_str
      basicc_str
      orignal_code

let info_code_part (state : Types.state) offset noffset =
  let pc = Cpu.get_pc state.arch.cpu in
  try
    print_program_header state.out_channel;
    for i=noffset to offset-1 do
      let addr = (pc + Int32.of_int i * 0x4l) in
      if addr >= 0l then (
        let code = Memory.get_int32 state.arch.memory  addr in

        if code = 0l
        then (fprintf state.out_channel "     End without syscall@."; raise Break)
        else print_addr state addr pc code
      )
    done
  with _ -> ()

let info_code_full (state : Types.state) =
  let pc   = Cpu.get_pc state.arch.cpu in
  let addr = ref 0l in
  let code = ref (Memory.get_int32 state.arch.memory !addr) in
  try
    print_program_header state.out_channel;
    while !code != 0l do
      print_addr state !addr pc !code;
      addr := !addr + 0x4l;
      code := Memory.get_int32 state.arch.memory !addr
    done;
    fprintf state.out_channel "     End without syscall@."
  with _ -> ()

let execute_info_code args (state : Types.state) =
  begin match args with
  | offset :: [] -> (
    try
      let offset  = int_of_string offset in
      info_code_part state offset 0
    with _ ->
      fprintf state.out_channel "%a Incorrect argument to print code@."
        error ())
  | offset :: noffset :: _ -> (
    try
      let offset  = int_of_string offset  in
      let noffset = int_of_string noffset in
      info_code_part state offset noffset
    with _ ->
      fprintf state.out_channel "%a Incorrect argument to print code@."
      error ())
  | _ -> info_code_full state
  end;
  state

let info_code : Types.cmd =
  { long_form   = "code";
    short_form  = "c";
    name        = "(c)ode";
    short_desc  = "Print code";
    long_desc   = ["Usage: info code <offset> <negative offset>"];
    execute     = execute_info_code;
    sub         = []; }

(* Memory ------------------------------------------------------------------- *)

let start_default = Segment.static_begin
let size_default  = 0x10

let line_size   = 0x4
let line_size32 = 0x4l

(* Return either reg as a Int32 or if it's a register the register content *)
let i32_or_reg_of_str reg (arch : Riscv.t) =
  try
    let r = Assembler.Regs.of_string reg in
    Cpu.get_reg arch.cpu (Int32.to_int r)
  with _ -> Int32.of_string reg

let print_line (state : Types.state) line_address =
  fprintf state.out_channel "0x%08x |" (int32_to_int line_address);
  for i = 0 to line_size - 1 do
    let addr  = line_address + (Int32.of_int i) in
    let value = Memory.get_byte state.arch.memory addr in
    fprintf state.out_channel "  %02lx" value
  done;
  fprintf state.out_channel " |\n"

let print_memory (state : Types.state) start size =
  fprintf state.out_channel "@{<bold>%-10s@} |  @{<bold>+0@}  @{<bold>+1@}  @{<bold>+2@}  @{<bold>+3@} |\n"
    "Address";
  let line_address = ref (Int32.logand start (Int32.lognot line_size32)) in
  for _ = 1 to size do
    print_line state (alignment !line_address);
    line_address := !line_address + line_size32
  done

let execute_info_memory args (state : Types.state) =
  begin match args with
  | []      -> print_memory state start_default size_default
  | [start] -> (
    try
      let start = i32_or_reg_of_str start state.arch in
      print_memory state start size_default
    with _ -> raise (Shell_error Bad_Usage))
  | [start; size] -> (
      try
        let start = i32_or_reg_of_str start state.arch in
        let size  = int_of_string size in
        print_memory state start size
      with _ -> raise (Shell_error Bad_Usage))
  | [start; size; nsize] -> (
      try
        let start = i32_or_reg_of_str start state.arch in
        let nsize = Int32.of_string nsize in
        let size  = int_of_string   size  in
        print_memory state (Int32.sub start nsize) size
      with _ -> raise (Shell_error Bad_Usage))
  | _ -> raise (Shell_error Bad_Usage)
  end;
  state

let info_memory : Types.cmd =
  { long_form   = "memory";
    short_form  = "m";
    name        = "(m)emory";
    short_desc  = "Print memory segment";
    long_desc   = [
              "Usage: info memory <start> <size> <negative size>";
              "<start>   Either an adress or a register containing an adress";
      sprintf "<size=%d> Size of the chunck of memory to print (forward)" size_default;
              "<nsize=0> Size of the chunck of memory to print (backward)";
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
  fprintf state.out_channel "@{<bold>%-9s@} | @{<bold>%-10s@} |\n" "Name" "Value"

let info_all_regs (state : Types.state) =
  print_reg_header state;
  for i = 0 to 31 do
    fprintf state.out_channel "%s | 0x%08x |\n" regs.(i)
      (int32_to_int (Cpu.get_reg state.arch.cpu i))
  done

let info_list_regs (state : Types.state) =
  print_reg_header state;
  List.iter (fun reg ->
    try
      if reg = "pc" then
        fprintf state.out_channel "pc | 0x%08x |\n"
          (int32_to_int (Cpu.get_pc state.arch.cpu))
      else
        let i =
          try Int32.to_int (Assembler.Regs.of_string reg)
          with _ -> int_of_string reg
        in
        fprintf state.out_channel "%s | 0x%08x |\n"
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

let info_registers : Types.cmd = {
  long_form   = "registers";
  short_form  = "r";
  name        = "(r)egisters";
  short_desc  = "Display value in specified registers";
  long_desc   = ["Usage: info registers <r_1> ... <r_n>"];
  execute     = execute_info_registers;
  sub         = [];
}

(* Decode ------------------------------------------------------------------- *)

let rec info : Types.cmd =
  { long_form   = "info";
    short_form  = "i";
    name        = "(i)nfo";
    short_desc  = "Print informations about CPU";
    long_desc   = [];
    execute     = (fun _ state -> Help.command info state);
    sub         = [info_memory; info_registers; info_code]; }
