# TODO

## 1.1.0

* Allow usage of program arguments: Use syntax `arrakis helloworld.s -- hello`
* Change alcotest and argsh to optional dependencies

### Improve shell

* Using up/down arrow to get last commands
* Using side arrow to modify current command
* Autocompletion of commands
* Handle syscall failure at shell level

## ??? (Future work without attributed planned version)

* openat (More detail in the file [scunix.ml](./arrakis/lib/syscall/scunix.ml))
* Would be nice to have better errors.
  Some that could be interesting would be 'Did you mean...' errors, for example
  when an instruction is misspelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out:
  ``Syntax Error on line .... bgt instruction usage: bgt rs, rt, offset.``
* Would be fun to have a small GIF in README displaying the use of breakpoints
  and printing stuff.
* Add a feedback for command 's': Print new pc
* Add support for other RISC-V extensions.
* Specify exactly which version of RISC-V ISA is currently implemented.
* Allow switching between multiple isa with a --isa option, taking a string as
  an argument that must be parsed. (Example: RV32IM, RV32I, ...).
* Add a new command to allow saving the logs of the current execution to a file
* Provide arrakis as system package for different linux distributions.

