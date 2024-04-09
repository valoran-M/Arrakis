# Common -----------------------------------------------------------------------
# Library of common utilities in RV32.
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

# Calculate the size of a string.
#   Parameters:
#     a0 : null-terminated string
#   Returns:
#     a0 : unchanged
#     a1 : size of a0
strlen:
  mv a1, a0
  0:
    lb   t0, 0(a1)
    beqz t0, 0f
    addi a1, a1, 1
    j 0b
  0:
  sub a1, a1, a0
  ret

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
