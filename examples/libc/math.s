# libc - math ------------------------------------------------------------------
# Implementation of a small subset of the libc in RV32IM.
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

# Returns the absolute value of a parameter
#   Parameters:
#     a0 : integer
#   Returns:
#     a0 : Absolute value a0
abs:
  bgtz a0 0f
  sub a0, x0, a0
  0:
  ret
