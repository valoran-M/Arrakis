# TODO

## 1.1.0

### New features

* Program arguments. Use syntax `arrakis helloworld.s --run hello world`
* [local labels](https://sourceware.org/binutils/docs/as/Symbol-Names.html)

### Improve shell

* Using up/down arrow to get last commands
* Using side arrow to modify current command
* Autocompletion of commands
* Handle syscall failure at shell level
* command clear shell (CTRL+L)
* Add argument to 'continue' command
* fixme: 'continue' does not seems to work
* fixme: Running 'run' when the current adress is a breakpoint shouldn't do
  anything
* fixme: Printing 'jal' instruction seems to be broken
* fixme: Long label can go out of 'original code' bounds
* Printing the content of 'sp' should also go backward
* When printing memory, show which case was the one which was asked so we can
  clearly see what is before and what is after
* Allow printing help for subcommand. 'help i m' should print out the help for
  'information memory'.
* Add feedback for command 's', 'c', 'r'...

## ??? (Future work without attributed planned version)

* Change `shell/types.ml > state` to be fully immutable and not use hashtbl
* openat (More detail in the file [scunix.ml](./arrakis/lib/syscall/scunix.ml))
* Would be nice to improve errors.
  Some that could be interesting would be 'Did you mean...' errors, for example
  when an instruction is misspelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out:
  ``Syntax Error on line .... bgt instruction usage: bgt rs, rt, offset.``
* Would be fun to have a small GIF in README displaying the use of breakpoints
  and printing stuff.
* Specify exactly which version of RISC-V ISA is currently implemented.
* Allow switching between multiple isa with a --isa option, taking a string as
  an argument that must be parsed. (Example: RV32IM, RV32I, ...).
* Add a new command to allow saving the logs of the current execution to a file
* Provide arrakis as system package for different linux distributions

