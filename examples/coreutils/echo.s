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
    lb   t0 0(a1)
    beqz t0, .while_exit_strlen
    addi a1, a1, 1
    j .while_strlen
  .while_exit_strlen:
  sub a1, a1, a0
  ret

fputs:
  sw ra, 0(sp)
  addi sp, sp, -4

  mv s0, a0
  call strlen
  mv a2, a1

  li a7, 64
  li a0, 1
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

  # Exit
  li a7, 93
  li a0, 0
  ecall
