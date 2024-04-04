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

fputs:
  addi sp, sp, -8
  sw   ra, 4(sp)
  sw   s0  8(sp)

  mv s0, a0
  call strlen
  mv a2, a0

  li a7, 64
  li a0, 1
  mv a1, s0
  ecall

  lw   ra, 4(sp)
  lw   s0  8(sp)
  addi sp, sp, 8
  ret

# Main -------------------------------------------------------------------------

.globl _start
_start:

  la a0, hello
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
