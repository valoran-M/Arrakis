# TODO

## Shell

* Breakpoint line may be hard to know. Printing line number could help.
* Using up arrow to get last command
* Using side arrow to modify current command

## Syscall

* openat (More detail in the file)

## Assembler

* Would be nice to have better errors.
  Some that could be interesting would be 'Did you mean...' errors, for exemple
  when an instruction is mispelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out:
  ``Syntax Error on line .... bgt instruction usage: bgt rs, rt, offset.``

## Diverse

* Would be fun to have a small gif in README displaying the use of breakpoints
  and printing stuff.
* Ocaml 4.14 support

