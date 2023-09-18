let general channel =
  Format.fprintf channel {|
  @{<fg_green>General help:@}

  @{<fg_green>*@} (r)un

    Run code.

 @{<fg_green>*@} (b)reakpoint

    Create breakpoint

 @{<fg_green>*@} (s)tep

    In debug mode: execute next instruction.

 @{<fg_green>*@} (pr)ev

    In debug mode: Recovery of the simulator's previous state

 @{<fg_green>*@} (n)ext

    Run to next breakpoint.

 @{<fg_green>*@} (p)rint

    Print informations about CPU.

 @{<fg_green>*@} (h)elp

    Show this help.

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

      default args:
        <start> : Starting data segement
        <nb>    : 0x10

  @{<fg_green>*@} (p)rint (r)egs <r_1> ... <r_n>

      Print specified registers.
      If the list is empty, display all of them.

  @{<fg_green>*@} (p)rint (c)ode <offset> <noffset>

      If no offset is specified, print code from start to finish.
      If offset is specified, print code from pc to pc+offset.
      If noffset is also specified, print code from pc-noffset to pc+offset.

@.|}

