# Pwd --------------------------------------------------------------------------
# Print name of working directory.
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

.data
  new_line: .ascii "\n"

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

# Main -------------------------------------------------------------------------

.globl _start
_start:
  addi sp, sp, -128

  # Get current working directory
  li   a7, 17
  mv   a0, sp
  li   a1, 128
  ecall
  beqz a0, error_exit

  # Print working directory
  call strlen
  mv a2, a1

  li  a7, 64
  li  a0, 1
  mv  a1, sp
  ecall
  bltz a0, error_exit

  # Print new line
  li  a7, 64
  li  a0, 1
  la  a1, new_line
  ecall
  bltz a0, error_exit

  # Exit
  li a7, 93
  li a0, 0
  ecall

# Exit with error code 1
error_exit:
  li a7, 93
  li a0, 1
  ecall
