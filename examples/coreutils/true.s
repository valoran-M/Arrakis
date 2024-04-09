# True -------------------------------------------------------------------------
# Do nothing with a 'success' exit status
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

.data

.text

.globl _start
_start:
  li a7, 93
  li a0, 0
  ecall
