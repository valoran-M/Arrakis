(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

exception History_Empty

type change =
  | Change_Memory_8  of int32 * int32 (* addresse, value *)
  | Change_Memory_16 of int32 * int32 (* addresse, value *)
  | Change_Memory_32 of int32 * int32 (* addresse, value *)
  | Change_Register  of int   * int32 (* register, value *)
  | Change_Nothing

type t


val create_history : unit -> t
  (** [create_history ()] create new empty history *)

val add_history : int32 -> change -> t -> t
  (** [add_history pc change history] Add a new item to the history *)

val step_back : Arch.t -> t -> t
  (** [step_back arch history] Resets the simulator to the previous state *)

val reset : Arch.t -> t -> t
  (** [reset arch history] Resets the simulator to the inital state *)

