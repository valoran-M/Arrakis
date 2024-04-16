# libc - string ----------------------------------------------------------------
# Implementation of a small subset of the libc in RV32IM.
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
