open Simulator

type syscall_ret = Exit of int | Continue

let print_int channel (arch : Arch.t) =
  Format.fprintf channel "%d" (Int32.to_int (Cpu.get_reg arch.cpu 11));
  Continue

let print_string _channel _arch = failwith "TODO"

let sbrk _arch = failwith "TODO"

let exit () = Exit 0

let print_character channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 11 in
  try
    let char = Char.chr (Int32.to_int reg) in
    Format.fprintf channel "%c" char;
    Continue
  with _ -> failwith "To big"

let exit2 (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 11 in
  Exit (Int32.to_int reg)

let syscall channel (arch : Arch.t) =
  let reg = Cpu.get_reg arch.cpu 10 in
  match reg with
  | 1l  -> print_int       channel arch
  | 4l  -> print_string    channel arch
  | 9l  -> sbrk                    arch
  | 10l -> exit            ()
  | 11l -> print_character channel arch
  | 17l -> exit2                   arch
  | _ ->
    Format.fprintf channel "Error : '%d' invalid syscall" (Int32.to_int reg);
    Continue
