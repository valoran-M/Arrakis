# TODO

## Shell

* `clear` command.
* `reset` command.
* Breakpoint line may be hard to know. Printing line number could help.
* Using up arrow to get last command

## Syscall

* sbrk syscall
* Directory management syscall (opendir, ...)

## Assembler

* Would be nice to have better errors.
  Some that could be interesting would be 'Did you mean...' errors, for exemple
  when an instruction is mispelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out:
  ``Syntax Error on line .... bgt instruction usage: bgt rs, rt, offset.``

## Diverse

* dune-project
* Would be fun to have a small gif in README displaying the use of breakpoints
  and printing stuff.
