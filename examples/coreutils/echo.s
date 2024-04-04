.data
  hello: .asciz "hello, world!\n"

.text

# Utils ------------------------------------------------------------------------

# Calculate the size of a string
# a0 : char*
# Returns:
# a0 : unchanged
# a1 : size of string
strlen:
  mv a1, a0
  .while_strlen:
    lb   t0 0(a1)
    beqz t0, .while_exit_strlen
    addi a1, a1, 1
    j .while_strlen
  .while_exit_strlen:
  sub a1, a1, a0
  ret

# Write a string to stdout
# a0 : char*
fputs:
  sw ra, 0(sp)
  addi sp, sp, -4

  mv s0, a0
  call strlen
  mv a2, a0

  li a7, 64   # write syscall
  li a0, 1    # stdout file descriptor
  mv a1, s0
  ecall

  lw ra, 4(sp)
  addi sp, sp, 4
  ret

# Main -------------------------------------------------------------------------

.globl _start
_start:

  la a0, hello
  call fputs

  # # TODO
  # # sp points to the number of arguments.
  # addi sp, 4
  # la sp t0
  # .while_start:
  #   beqz t0, .while_exit_start
  #   addi sp, 4
  # .while_exit_start:

  # exit
  li a7, 93
  li a0, 0
  ecall
