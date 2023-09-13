.data

  stack_size: .word 1024
  stack:      .zero 1024

  line_size: .word 256
  line:      .zero 256


# /!\ Norms for this assembly code
#
# s1 : calculator's stack pointer
# s2 : calculator's stack size

.text

# Stack ########################################################################

# Initialise stack
init_stack:
      la s1, stack
      li s2, 0
      ret

# Push first argument in stack
# a0 : argument to push
# return :
# a0 : 1 if succeed else 0
push_stack:
      lw t0, stack_size
      bge s2, t0, .stack_full
      sw a0, 0(s1)
      addi s1, s1, 4
      addi s2, s2, 4
      li a0, 1
      ret
  .stack_full:
      li a0, 0
      ret

# Pop first argument of the stack
#
# return:
# a0 : 1 if succeed else 0
# a1 : pop number
pop_stack:
      blez s2, .stack_empty
      lw a1, -4(s1)
      addi s1, s1, -4
      addi s2, s2, -4
      li a0, 1
      ret
  .stack_empty:
      li a0, 0
      ret

# input ########################################################################

# Read line and store then to line address (.data)
#
# return:
# a0 : line size
get_line:
      li a7, 63
      li a0, 0
      la a1, line
      lw a2, line_size
      ecall
      ret

read_line:
      # push ra
      sw ra, 0(sp)
      addi sp, sp, -4

      call get_line

      # pop ra
      lw ra, 4(sp)
      addi sp, sp, 4
      ret

# Main #########################################################################

.globl main
main:
      call init_stack

      call read_line
  
      # exit
      li a7, 93
      li a0, 0
      ecall

