# Errors

* Lexical errors

* Syntax error

* Unknown label

* Interval imm

* Running in root mode is not allowed!
    See [here](./options.md#some-additionnal-information).

# Warning

* Exiting without an exit syscall
    The `pc` register is pointing to a part of the program without any instructions.
    The program has ended but no proper system call was made to terminate it
    properly.

