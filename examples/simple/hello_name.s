.data
  hello:   .ascii "Hello, "

.text

.globl _start
_start:

  addi sp, sp, -15  # Allocate place in the stack to store the name

  li a7, 63     # Load the read syscall ID (63)
  li a0, 0      # Read from stdin (File descriptor 0)
  mv a1, sp     # Store result in the stack
  li a2, 15     # Max read size
  ecall

  mv t0, a0     # We keep the number of byte read in t0

  li a7, 64     # Load the write syscall ID (64)
  li a0, 1      # Write into stdout (File descriptor 1)
  la a1, hello
  li a2, 7      # The hello label is 7 byte wide
  ecall

  li a0, 1      # Write into stdout (File descriptor 1)
  mv a1, sp     # Writing from our allocated space in the stack
  mv a2, t0     # t0 is storing the size of the name
  ecall

  li a7, 93     # Load the exit syscall ID (93)
  li a0, 0      # Use exit code 0, indicating sucess
  ecall

