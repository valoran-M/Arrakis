# Echo -------------------------------------------------------------------------
# Display a line of text.
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

.data

.text

# Utils ------------------------------------------------------------------------
# A more fitted version of common utilities from their counterpart in 'common.s'

# Calculate the size of a string.
#   Parameters:
#     a1 : null-terminated string
#   Returns:
#     a1 : unchanged
#     a2 : size of a0
strlen:
  mv a2, a1
  0:
    lb   t0, 0(a2)
    beqz t0, 0f
    addi a2, a2, 1
    j 0b
  0:
  sub a2, a2, a1
  ret

# Write a null terminated string to stdout.
#   Parameters:
#     a1 : null-terminated string
fputs:
  addi sp, sp, -4
  sw   ra, 0(sp)

  call strlen

  li a7, 64
  li a0, 1
  ecall

  lw   ra, 0(sp)
  addi sp, sp, 4
  ret

# Write a character to stdout
#   Parameters:
#     a1 : character
putchar:
  addi sp, sp, -4
  sw   a1, 0(sp)

  li a7, 64
  li a0, 1
  mv a1, sp
  li a2, 1
  ecall

  addi sp, sp, 4
  ret

# Main -------------------------------------------------------------------------

.globl _start
_start:

  # We could use directly sp here instead of s0 as we don't care
  # about destroying arguments once used
  addi s0, sp, 4
  0:
    lw   a1, 0(s0)
    beqz a1, 0f
    call fputs

    # Print a space in between each string
    li   a1, ' '
    call putchar

    addi s0, s0, 4
    j 0b
  0:

  li   a1, '\n'
  call putchar

  # Exit
  li a7, 93
  li a0, 0
  ecall

