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
# A more fitted version of common utilities from their counterpart in 'common.s'

# Calculate the size of a string.
#   Parameters:
#     a0 : null-terminated string
#   Returns:
#     a0 : unchanged
#     a2 : size of a0
strlen:
  mv a2, a0
  .while_strlen:
    lb   t0, 0(a2)
    beqz t0, .while_exit_strlen
    addi a2, a2, 1
    j .while_strlen
  .while_exit_strlen:
  sub a2, a2, a0
  ret

# Main -------------------------------------------------------------------------

.globl _start
_start:
  addi sp, sp, -256

  # Get current working directory
  li   a7, 17
  mv   a0, sp
  li   a1, 256
  ecall
  beqz a0, error_exit

  # Print working directory
  call strlen

  li  a7, 64
  li  a0, 1
  mv  a1, sp
  ecall
  bltz a0, error_exit

  # Print new line
  li a0, 1
  la a1, new_line
  li a2, 1
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
