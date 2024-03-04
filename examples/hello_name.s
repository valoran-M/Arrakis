.data
  hello:       .ascii "Hello, "
  exclamation: .ascii "!\n"
  maxname:     15

.text

.globl _start

_start:

  lb t0, maxname

  sub sp, sp, t0    # We store the name in the stack

  li a7, 63         # Load the read syscall ID (63)
  li a0, 0          # Read from stdin (File descriptor 0)
  mv a1, sp         # Store result in the stack
  mv a2, t0         # Max read size
  ecall

  mv   t0, a0       # We keep the number of byte read in t0
  addi t0, t0, -2   # Remove the trailing newline

  li a7, 64         # Load the write syscall ID (64)
  li a0, 1          # Write into stdout (File descriptor 1)
  la a1, hello
  li a2, 7          # The hello label is 7 byte wide
  ecall

  li a0, 1          # Write syscall will have overwritten a0
  mv a1, sp
  mv a2, t0
  ecall

  li a0, 1          # Write syscall will have overwritten a0
  la a1, exclamation
  li a2, 2
  ecall

  # Exit the program with a proper syscall
  li a7, 93
  li a0, 0
  ecall

