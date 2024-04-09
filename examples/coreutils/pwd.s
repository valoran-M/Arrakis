# Pwd --------------------------------------------------------------------------
# Print name of working directory.
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

.data
  new_line: .ascii "\n"

.text

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
  # getcwd syscall returns the size written including trailing '\0',
  # which we don't need to write
  addi a2, a0, -1
  li   a7, 64
  li   a0, 1
  mv   a1, sp
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
