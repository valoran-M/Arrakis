# libc - stdio -----------------------------------------------------------------
# Implementation of a small subset of the libc in RV32IM.
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

# Write a null terminated string to stdout.
#   Parameters:
#     a0 : null-terminated string
fputs:
  addi sp, sp, -4
  sw   ra, 0(sp)

  call strlen
  mv a2, a0

  li a7, 64
  li a0, 1
  mv a1, a0
  ecall

  lw   ra, 0(sp)
  addi sp, sp, 4
  ret

# Write a character to stdout
#   Parameters:
#     a0 : character
putchar:
  addi sp, sp, -4
  sw   a0, 0(sp)

  li a7, 64
  li a0, 1
  mv a1, sp
  li a2, 1
  ecall

  addi sp, sp, 4
  ret
