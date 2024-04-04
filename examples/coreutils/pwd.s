

# Main -------------------------------------------------------------------------

.globl _start
_start:
  li   a7, 17
  addi sp, sp, -30
  add  a0, x0, sp
  li   a1, 30
  ecall

  li  a7, 64
  li  a0, 1
  add a1, x0 sp
  li  a2, 30 # TODO: Change to use a strlen function
  ecall

  # TODO: Print a trailing new line character

  # exit
  li a7, 93
  li a0, 0
  ecall
