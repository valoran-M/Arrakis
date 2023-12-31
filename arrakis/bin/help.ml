(******************************************************************************)
(* Copyright 2023 - Arrakis contributors                                      *)
(*                                                                            *)
(* This file is part of Arrakis, a RISC-V simulator.                          *)
(* It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>    *)
(******************************************************************************)

let general channel =
  Format.fprintf channel {|
  @{<fg_green>General help:@}

  @{<fg_green>*@} (h)elp

    Show this help.

  @{<fg_green>*@} (b)reakpoint

    Create breakpoints.

  @{<fg_green>*@} (s)tep

    Execute next instruction.

  @{<fg_green>*@} (pre)v

    Recovery of the simulator's previous state.

  @{<fg_green>*@} (res)et

    Recovery of the simulator's initial state.

  @{<fg_green>*@} (n)ext

    Run to next breakpoint.

  @{<fg_green>*@} (r)un

    Run code until the end.

  @{<fg_green>*@} (p)rint

    Print informations about CPU.

  @{<fg_green>*@} (q)uit
@.|}

let breakpoint channel =
  Format.fprintf channel {|
  @{<fg_green> Breakpoint help:@}

  @{<fg_green>*@} (b)reakpoint (l)ine <l_1> ... <l_n>

    Set breakpoints on specified lines.

  @{<fg_green>*@} (b)reakpoint (a)ddr <b_1> ... <b_n>

    Set breakpoints on specified addresses.

  @{<fg_green>*@} (b)reakpoint (r)emove <b_1> ... <b_n>

    Remove specified breakpoints.

  @{<fg_green>*@} (b)reakpoint (p)rint

    Print all breakpoints.
@.|}

let print channel =
  Format.fprintf channel {|
  @{<fg_green>Print help:@}

  @{<fg_green>*@} (p)rint (m)emory <start> <nb>

      Print memory segment.
      Starts at address <start> and displays <nb> 32 bits.

      <start> can either be an adress or a register.
      If it's a register, the actual start value will be the value stored inside
      the register.

      @{<fg_yellow>default args:@}
        <start> : Starting data segement
        <nb>    : 0x10

  @{<fg_green>*@} (p)rint (r)egs <r_1> ... <r_n>

      Display value in specified registers.
      If no register are specified, display all of them.

  @{<fg_green>*@} (p)rint (c)ode <offset> <noffset>

      If no offset is specified, print code from start to finish.
      If offset is specified, print code from pc to pc+offset.
      If noffset is also specified, print code from pc-noffset to pc+offset.

@.|}

