type ret = string

let command = "(p)"

let description = "Print informations about CPU."

let help () =
  Format.sprintf {|
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

    Display value in specified registers.
    If no register are specified, display all of them.

@{<fg_green>*@} (p)rint (c)ode <offset> <noffset>

    If no offset is specified, print code from start to finish.
    If offset is specified, print code from pc to pc+offset.
    If noffset is also specified, print code from pc-noffset to pc+offset.

@.|}

let exec _ = ""

