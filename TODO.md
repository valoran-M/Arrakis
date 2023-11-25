# TODO

## New features

* Allow usage of program arguments
* Add a '--version / -v' argument to print Arrakis version.

## Shell

* Using up arrow to get last command
* Using side arrow to modify current command

## Syscalls

* openat (More detail in the file [scunix.ml](./arrakis/lib/syscall/scunix.ml]))

## Assembler

* Would be nice to have better errors.
  Some that could be interesting would be 'Did you mean...' errors, for example
  when an instruction is misspelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out:
  ``Syntax Error on line .... bgt instruction usage: bgt rs, rt, offset.``

* Add support for other RISC-V extensions.

## Other

* Would be fun to have a small GIF in README displaying the use of breakpoints
  and printing stuff.

## Fix

* Alignment of program argument seems currently broken. It has currently been
  disabled but should be enabled again when it is fixed.

