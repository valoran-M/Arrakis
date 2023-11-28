# TODO

## ???

* openat (More detail in the file [scunix.ml](./arrakis/lib/syscall/scunix.ml]))
* Would be nice to have better errors.
  Some that could be interesting would be 'Did you mean...' errors, for example
  when an instruction is misspelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out:
  ``Syntax Error on line .... bgt instruction usage: bgt rs, rt, offset.``
* Add support for other RISC-V extensions.
* Would be fun to have a small GIF in README displaying the use of breakpoints
  and printing stuff.
* Remove alcotest dependency

## 1.1.0

* Allow usage of program arguments: Use syntax arrakis helloworld.s -- hello

### Improve shell user experience

* Using up/down arrow to get last commands
* Using side arrow to modify current command
* Autocompletion of commands

## 1.0.1

* Zsh and Bash completion
* Clean up code

