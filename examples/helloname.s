.data
  hello:       .asciz "Hello, "
  exclamation: .asciz "!\n"
  maxname:     15

.text

.globl _start

_start:

  lb t0, maxname

  sub sp, sp, t0    # We are going to store the name of the user in the stack.

  li a7, 63         # Load the read syscall ID (63)
  li a0, 0          # Read from stdin (File descriptor 0)
  add a1, x0, sp    # We are storing the name in the stack
  add a2, x0, t0
  ecall

  add t0, x0, a0    # We keep the number of byte read away
  addi t0, t0, -1   # Remove the trailing newline

  li a7, 64         # Load the write syscall ID (64)
  li a0, 1          # Write into stdout (File descriptor 1)
  la a1, hello
  li a2, 7          # The hello label is 7 byte wide
  ecall

  li a7, 64
  li a0, 1
  add a1, x0, sp
  add a2, x0, t0
  ecall

  li a0, 1
  la a1, exclamation
  li a2, 2
  ecall

  # Exit the program with a proper syscall
  li a7, 93
  li a0, 0
  ecall

