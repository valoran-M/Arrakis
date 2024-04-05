# Echo -------------------------------------------------------------------------
# Display a line of text.
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

.data
  hello: .asciz "hello, world!\n"

.text

# Utils ------------------------------------------------------------------------
# Copied straight from 'common.s' for now. See documentation over there.

# Return string length
# Parameters :
#   a1 : string
# Return
#   a2 : length
strlen:
  mv a2, a1
  .while_strlen:
    lb   t0, 0(a2)
    beqz t0, .while_exit_strlen
    addi a2, a2, 1
    j .while_strlen
  .while_exit_strlen:
  sub a2, a2, a1
  ret

# Print a1
fputs:
  sw   ra, 0(sp)
  addi sp, sp, -4

  call strlen

  li a7, 64
  li a0, 1
  ecall

  addi sp, sp, 4
  lw   ra, 0(sp)
  ret

# Main -------------------------------------------------------------------------

.globl _start
_start:

  la a1, hello
  call fputs

  # mv s0, sp
  # .while_start:
  #   lw   t0, 0(s0)
  #   beqz t0, .while_exit_start
  #   mv a0, s0
  #   call fputs
  #   addi s0, s0, 4
  # .while_exit_start:

  # Exit
  li a7, 93
  li a0, 0
  ecall
