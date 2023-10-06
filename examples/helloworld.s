.data
  hello:  .asciz "Hello World!\n"

.text

.globl _start

_start:
  li a7, 64     # Load the write syscall ID (64)
  li a0, 1      # Write into stdout (File descriptor 1)
  la a1, hello  # Load the string "hello, world!" into a1 (Buffer to write)
  li a2, 13     # Load the size of "hello" into a2 (Number of byte to write)
  ecall

  # Exit the program with a proper syscall
  li a7, 93     # Load the exit syscall ID (93)
  li a0, 0      # Use exit code 0 (Meaning that the program exited sucessfully)
  ecall

