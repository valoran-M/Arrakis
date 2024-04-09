# TODO

## 1.1.0

### Follow GNU as syntax

* Some assembler directive are free to add and should therefore probably be
  added
* Add expression [Section 6: expression](https://sourceware.org/binutils/docs-2.42/as.pdf)

### Improve shell

* Using up/down arrow to get last commands
* Using side arrow to modify current command
* Autocompletion of commands
* Handle syscall failure at shell level
* Command clear shell (CTRL+L)
* Long label can go out of 'original code' bounds
* When printing memory, show which case was the one which was asked so we can
  clearly see what is before and what is after
* Add feedback for command 's', 'c', 'r'...
* Add an error when program is not running and running 'c' or 's'

## ??? (Future work without attributed planned version)

* openat (More detail in the file [scunix.ml](./arrakis/lib/syscall/scunix.ml))
* Would be nice to improve errors.
  Some that could be interesting would be 'Did you mean...' errors, for example
  when an instruction is misspelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out:
  ``Syntax Error on line .... bgt instruction usage: bgt rs, rt, offset.``
* Specify exactly which version of RISC-V ISA is currently implemented.
* Allow switching between multiple isa with a --isa option, taking a string as
  an argument that must be parsed. (Example: RV32IM, RV32I, ...).
* Add a new command to allow saving the logs of the current execution to a file
* Provide arrakis as system package for different linux distributions
* F extensions
* Allow using arrakis with multiple files

