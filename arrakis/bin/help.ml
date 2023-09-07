let print_help channel =
  Format.fprintf channel {|
  @{<fg_green>General help:@}

  @{<fg_green>*@} (r)un

    Run code.

 @{<fg_green>*@} (b)reakpoint

    Create breakpoint

 @{<fg_green>*@} (s)tep

    In debug mode: execute next instruction.

 @{<fg_green>*@} (n)ext

    Run to next breakpoint.

 @{<fg_green>*@} (p)rint

    Print informations about CPU.

 @{<fg_green>*@} (h)elp

    Show this help.

 @{<fg_green>*@} (q)uit
@.|}

let help_breakpoint channel =
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

let print_memory_help channel =
  Format.fprintf channel {|
  @{<fg_green>Print help:@}

  @{<fg_green>*@} (p)rint (m)emory <start> <nb>

      Print memory segment.
      Starts at address <start> and displays <nb> 32 bits.

      default args:
        <start> : start data segement
        <nb>    : 0x10

  @{<fg_green>*@} (p)rint (r)egs <r_1> ... <r_n>

      Print specified registers.
      If the list is empty, display all of them.

      Accepted register may be x0...x31 or zero, ra, ...

  @{<fg_green>*@} (p)rint (c)ode offset

      Print code from pc value to offset.
@.|}

