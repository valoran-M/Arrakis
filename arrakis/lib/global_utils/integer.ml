(******************************************************************************)
(* Copyright 2023-2024 - Arrakis contributors                                 *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

open Error

let int32_to_int i =
  match Int32.unsigned_to_int i with
  | Some i -> i
  | None   -> raise (Utils_error Conversion_Failure)

let sign_extended i size =
  Int32.shift_right (Int32.shift_left i (32 - size)) (32 - size)

let alignment i = Int32.sub i (Int32.rem i 4l)

let char_to_int32 c = Int32.of_int (Char.code c)
let char_of_int32 c = Char.chr     (Int32.to_int c)
