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
  .while_strlen:
    lb   t0, 0(a1)
    beqz t0, .while_exit_strlen
    addi a1, a1, 1
    j .while_strlen
  .while_exit_strlen:
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

