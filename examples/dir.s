.data
  path:        .asciz "/home/"

.text

.globl _start

# Change the working directory to the "/home/" directory,
# print the working directory and then exit.
_start:

  li a7, 49    # Load the chdir syscall ID
  la a0, path  # Specify path to change dir to
  ecall

  # Note: We are not handling the error that could be returned by the 'chdir'
  # syscall here.
  # On failure, a0 would be storing value -1

  addi sp, sp, -50  # Increase stack size

  li a7, 17         # Load the getcwd syscall ID
  add a0, zero, sp  # We're storing the current working directory in the stack
  addi a1, zero, 50 # We have a buffer of 50 bytes
  ecall

  # Note: We are also not handling the error that may have been returned from
  # the 'getcwd' syscall.
  # If buffer is too small, it may have returned 0 in a0.

  li a7, 64         # Load the write syscall id
  li a0, 1          # Write to STDOUT
  add a1, zero, sp  # Source is the stack
  addi a2, zero, 50 # We're writing 50 bytes
  ecall

  # Exit the program with a proper syscall
  li a7, 93 # Load exit syscall
  li a0, 0  # 0 indicate sucess.
  ecall

