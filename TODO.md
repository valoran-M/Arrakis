# TODO

## Simulator

* Better test for RV32M Multiply extension
* Add RV32F Floating Point extension

## Shell

* A `clear` command would be nice to have.

## Assembler

* Pseudo-Instruction: Try to figure out a clean way to handle them.
  Problem about them: Some have the same name as normal instructions, and as
  such they are parsed like normal instruction instead of being able to count as
  both.
  Some way to handle this may be to turn everything around, have a token for
  each instruction in the parser, and a huge hashtbl associating everything
  correctly in the lexer.
  Would ease the lexer a lot but would add a lot of strain on the parser.
* Would be nice to have better errors.
  Some that could be interesting would be 'Did you mean...' errors, for exemple
  when an instruction is mispelled.
  If there is a start of instruction and the rest is not correct, an interesting
  error would be to show how the instruction should be used.
  For example, ``bgt rs`` would give out: ``Parsing Error on line .... bgt
  instruction usage: bgt rs, rt, offset.``

## Diverse

* Documentation
* Sys Calls
* dune-project
* Would be fun to have a small gif in README displaying the use of breakpoints
  and printing stuff.
