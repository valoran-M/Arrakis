(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Simulator
open Disassembler
open Format

exception Break

let ( + ) = Int32.add
let ( * ) = Int32.mul

(* Progam ------------------------------------------------------------------  *)

let print_program_header channel =
  fprintf channel "%3s | %2s%2s @{<bold>%10s@} | @{<bold>%12s@} | @{<bold>%-18s@} | @{<bold>%-18s@} |\n"
    "" "" "" "Adress" "Machine Code" "Basic Code" "Original Code"

let print_addr (state : Types.state) addr pc code =
    let breakpoint_str  =
      try  sprintf "%02d" (Hashtbl.find state.breakpoints addr)
      with Not_found -> ""
    in
    let addr_pc               = if addr = pc then ">" else ""              in
    let addr_str              = Utils.int32_to_int addr                    in
    let addr_str              = sprintf "0x%08x" addr_str                  in
    let machinec_str          = Utils.int32_to_int code                    in
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

let print_code_part (state : Types.state) offset noffset =
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

let print_code_full (state : Types.state) =
  let pc = Cpu.get_pc state.arch.cpu in
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

let decode_code_args args (state : Types.state) =
  match args with
  | offset :: [] -> (
    try
      let offset  = int_of_string offset in
      print_code_part state offset 0
    with _ ->
      fprintf state.out_channel "@{<fg_red>Error:@} Incorrect argument to print code.@.")
  | offset :: noffset :: _ -> (
    try
      let offset  = int_of_string offset  in
      let noffset = int_of_string noffset in
      print_code_part state offset noffset
    with _ ->
      fprintf state.out_channel "@{<fg_red>Error:@} Incorrect argument to print code.@.")
  | _ -> print_code_full state

(* Memory ------------------------------------------------------------------- *)

let start_default = Segment.static_being
let size_default  = 0x10

let line_size   = 0x4
let line_size32 = 0x4l

(* Return either reg as a Int32 or if it's a register the register content *)
let i32_or_reg_of_str reg (arch : Arch.t) =
  try
    let r = Assembler.Regs.of_string reg in
    Cpu.get_reg arch.cpu (Int32.to_int r)
  with _ -> Int32.of_string reg

let print_line (state : Types.state) line_address =
  fprintf state.out_channel "0x%08x" (Utils.int32_to_int line_address);
  for i = line_size - 1 downto 0 do
    let addr  = line_address + (Int32.of_int i)  in
    let value = Memory.get_byte state.arch.memory addr in
    fprintf state.out_channel "  %02x" (Int32.to_int value)
  done;
  fprintf state.out_channel "@."

let print_memory (state : Types.state) start size =
  fprintf state.out_channel " Address    +3  +2  +1  +0\n";
  let line_address = ref (Int32.logand start (Int32.lognot line_size32)) in
  for _ = 1 to size do
    print_line state !line_address;
    line_address := !line_address + line_size32
  done

let decode_memory_args args (state : Types.state) =
  match args with
  | []      -> print_memory state start_default size_default
  | [start] -> (
    try
      let start = i32_or_reg_of_str start state.arch in
      print_memory state start size_default
    with _ ->
        printf "@{<fg_red>Error:@} Incorrect argument to print memory.@.")
  | [start; size] -> (
      try
        let start = i32_or_reg_of_str start state.arch in
        let size  = int_of_string size in
        print_memory state start size
      with _ ->
        printf "@{<fg_red>Error:@} Incorrect argument to print memory.@.")
  | _ -> ()

(* Regs --------------------------------------------------------------------- *)

let regs = [|
  "    zero"; " ra (x1)"; " sp (x2)"; " gp (x3)";
  " tp (x4)"; " t0 (x5)"; " t0 (x6)"; " t1 (x7)";
  " s0 (x8)"; " s1 (x9)"; "a0 (x10)"; "a1 (x11)";
  "a2 (x12)"; "a3 (x13)"; "a4 (x14)"; "a5 (x15)";
  "a6 (x16)"; "a7 (x17)"; "s2 (x18)"; "s3 (x19)";
  "s4 (x20)"; "s5 (x21)"; "s6 (x22)"; "s7 (x23)";
  "s8 (x24)"; "s9 (x25)"; "s10(x26)"; "s11(x27)";
  "t3 (x28)"; "t4 (x29)"; "t5 (x30)"; "t6 (x31)";
|]

let print_all_regs (state : Types.state) =
  for i = 0 to 31 do
    fprintf state.out_channel "  %s -> 0x%08x\n" regs.(i)
      (Simulator.Utils.int32_to_int (Cpu.get_reg state.arch.cpu i))
  done

let print_list_regs (state : Types.state) =
  List.iter (fun reg ->
    try
      if reg = "pc" then
        fprintf state.out_channel
          "  pc -> 0x%08x\n"
          (Simulator.Utils.int32_to_int (Cpu.get_pc state.arch.cpu))
      else
        let i =
          try Int32.to_int (Assembler.Regs.of_string reg)
          with _ -> int_of_string reg
        in
        fprintf state.out_channel
          "  %s -> 0x%08x\n" regs.(i)
          (Simulator.Utils.int32_to_int (Cpu.get_reg state.arch.cpu i))
    with _ ->
      fprintf state.out_channel
        "@{<fg_red>Error@}: '%s' isn't a register.@." reg
  )

let decode_regs_args args (state : Types.state) =
  match args with
  | [] -> print_all_regs  state
  | _  -> print_list_regs state args

(* Decode ------------------------------------------------------------------- *)

let execute args (state : Types.state) =
  begin match args with
  | "m" :: args | "memory" :: args -> decode_memory_args args state
  | "r" :: args | "regs"   :: args -> decode_regs_args   args state
  | "c" :: args | "code"   :: args -> decode_code_args   args state
  | _                              -> Help.print state.out_channel
  end;
  state

let print : Types.command = {
  long_form   = "print";
  short_form  = "p";
  name        = "(p)rint";
  description = "Print informations about CPU.";
  execute;
}
