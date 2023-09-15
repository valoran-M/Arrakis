.data
  hello:  .asciz "Hello World!\n"

.text

.globl main

main:
  li a7, 64     # Load the write syscall ID (64)
  li a0, 1      # Write into stdout (File descriptor 1)
  la a1, hello  #
  li a2, 13     #
  ecall

  # Exit the program with a proper syscall
  li a7, 93
  li a0, 0
  ecall

