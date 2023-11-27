# TODO

## New features

* Allow usage of program arguments

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
* Zsh and Bash completion

## Fix

* Altough only one input file can currently be used, no warking are specified if
  multiple are passed in arguments. Should change "input_file" to a string
  option and exit with an error if multiple files are given.

