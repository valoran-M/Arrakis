open Simulator
open Disassembler

exception Break

let ( + ) = Int32.add
let ( * ) = Int32.mul

(* Progam ------------------------------------------------------------------  *)

let print_prog (arch : Arch.t) code_print debug =
  let pc = Cpu.get_pc arch.cpu in
  try
  Format.printf "   Adress\t\tMachine Code\t\tBasic Code\t\tOriginal Code@.";
  for i=0 to code_print-1 do
    let addr = (pc + Int32.of_int i * 0x4l) in
    let code = Memory.get_int32 arch.memory  addr in

    if code = 0l
    then (print_endline "   End without syscall"; raise Break)
    else let _, orignal_code = Hashtbl.find debug addr in
         Format.printf "%s 0x%08x\t\t0x%08x\t\t%-24s%s@."
          (if i = 0 then "->" else "  ") (Utils.int32_to_int addr)
          (Utils.int32_to_int  code) (print_code arch code) orignal_code
  done
  with _ -> ()

(* Memory ------------------------------------------------------------------- *)

let start_default = Segment.static_being
let size_default  = 0x10

let line_size   = 0x4
let line_size32 = 0x4l

let print_line (arch: Arch.t) line_address =
  Format.printf "0x%08x" (Utils.int32_to_int line_address);
  for i = line_size - 1 downto 0 do
    let addr = line_address + (Int32.of_int i) in
    let value = Memory.get_byte arch.memory addr in
    Format.printf "  %02x" (Int32.to_int value)
  done;
  print_newline ()

let print_memory (arch: Arch.t) start size =
  print_endline " Address    +3  +2  +1  +0";
  let line_address = ref (Int32.logand start (Int32.lognot line_size32)) in
  for _ = 1 to size do
    print_line arch !line_address;
    line_address := !line_address + line_size32
  done

let decode_memory_arguments (arch: Arch.t) args =
  match args with
  | [] -> print_memory arch start_default size_default
  | [start] -> print_memory arch (Int32.of_string start) size_default
  | [start; size] ->
    print_memory arch (Int32.of_string start) (int_of_string size)
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

let print_all_regs (arch: Arch.t) =
  for i = 0 to 31 do
    Format.printf "  %s -> %08x\n" regs.(i)
      (Int32.to_int (Cpu.get_reg arch.cpu i))
  done

let print_list_regs (arch: Arch.t) =
  List.iter (fun reg ->
    try
      let i = int_of_string reg in
      Format.printf "  %s -> %08x\n" regs.(i)
        (Int32.to_int (Cpu.get_reg arch.cpu i))
    with _ -> Format.printf "@{<fg_red>Error@}: \"%s\" isn't a register@." reg
  )

let decode_regs_arguments (arch: Arch.t) args =
  match args with
  | [] -> print_all_regs arch
  | _ -> print_list_regs arch args


(* Decode ------------------------------------------------------------------- *)

let print_memory_help () =
  print_string {|
  Print command :

  * (p)rint (m)emory <start> <nb>

      Print memory segement, it starts at address <start>
      and displays <nb> 32 bits

      default:
        <start> : start data segement
        <nb>    : 0x10

  * (p)rint (r)egs <r1> ...

      Print regs list, if the list is empty, desplays all registers

  * (p)rint (c)ode offset

      Print code, from pc value to offet
|}

let decode_print arch args addr_debug =
  match args with
  | "m" :: l | "memory" :: l -> decode_memory_arguments arch l
  | "r" :: l | "regs"   :: l -> decode_regs_arguments   arch l
  | ["c"; o] | ["code"; o]   ->
    (try print_prog arch (int_of_string o) addr_debug with _ -> ())
  | _ -> print_memory_help ()
