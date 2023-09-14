.data

  stack_size: .word 1024
  stack:      .zero 1024

  line_size: .word 256
  line:      .zero 256

  plus: .asciiz "plus\n"

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

# Operator #####################################################################

exec_add:
      # push ra
      sw ra, 0(sp)
      addi sp, sp, -4

      li a7, 64
      li a0, 1
      la a1, plus
      li a2, 6
      ecall

      # pop ra
      lw ra, 4(sp)
      addi sp, sp, 4
      ret

# input ########################################################################

# Skip empty character
# a0 : string address
#
# return:
# a0 : new string pointer
skip_space:
  .while_skip_space:
      lb t0, 0(a0)
      xori t1, t0, 9    # 0 if t1 = '\t'
      beqz t1, .lazy_or
      xori t0, t0, 32   # 0 if t1 = ' '
      bnez t0, .end_skip_space
    .lazy_or:
      addi a0, a0, 1
      j .while_skip_space
  .end_skip_space:
      ret

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

exec_op:
      # push ra
      sw ra, 0(sp)
      addi sp, sp, -4
      la s2, line

    .while_exec:
      mv a0, s2
      call skip_space
      lb t1, 0(s2)

      beqz t1, .zero_exec

      xori t0, t1, 43     # if t1 = '+'
      beqz t0, .add_op
      j .continue_exec

    .add_op:
      call exec_add

    .continue_exec:
      addi s2, s2, 1
      j .while_exec
    .zero_exec:
      call get_line
      la s2, line
      j .while_exec
    .end:
      # pop ra
      lw ra, 4(sp)
      addi sp, sp, 4
      ret

# Main #########################################################################

.globl main
main:
      call init_stack

      call exec_op

      # exit
      li a7, 93
      li a0, 0
      ecall

