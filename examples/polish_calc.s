.data

  stack_size: .word 1024
  stack:      .zero 1024

  line_size: .word 256
  line:      .zero 256

  print:     .zero 5
             .asciiz "\n"

  plus: .asciiz "plus\n"

# /!\ Norms for this assembly code
#
# s1 : calculator's stack pointer
# s2 : calculator's stack size
# s3 : line's address

.text

# Utils ########################################################################

# Print register
# a0 : register to print
print_reg:
      la a1, print
      addi a1, a1, 19
      li a2, 2
      li t1, 10
      .while_print_reg:
          rem t2, a0, t1
          addi t2, t2, 48
          sb t2, 0(a1)
          div a0, a0, t1
          addi a1, a1, -1
          addi a2, a2, 1
          bnez a0, .while_print_reg
      li a7, 64
      li a0, 1
      ecall
      ret

# test if a character is a digit
# a0 : character
#
# return :
# a0 : digit if is a digit else -1
is_digit:
      li t0, 48
      blt a0, t0, .false_digit
      li t0, 58
      bge a0, t0, .false_digit
      addi a0, a0, -48
      ret
    .false_digit:
      li a0, -1
      ret

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

# Get first element of the stack
#
# return:
# a0 : 1 if succeed else 0
# a1 : first argument
first_stack:
      blez s2, .stack_empty
      lw a1 -4(s1)
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

      call pop_stack
      mv t0, a1
      call pop_stack
      add a0, t0, a1
      call push_stack

      # pop ra
      lw ra, 4(sp)
      addi sp, sp, 4
      ret

exec_sub:
      # push ra
      sw ra, 0(sp)
      addi sp, sp, -4

      call pop_stack
      mv t0, a1
      call pop_stack
      sub a0, t0, a1
      call push_stack

      # pop ra
      lw ra, 4(sp)
      addi sp, sp, 4
      ret

exec_mult:
      # push ra
      sw ra, 0(sp)
      addi sp, sp, -4

      call pop_stack
      mv t0, a1
      call pop_stack
      mul a0, t0, a1
      call push_stack

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

# Read line for get an integer read on (s3) string
# a0 : first digit
#
# return:
# a0 : digit
get_digit:
      # push ra
      sw ra, 0(sp)
      addi sp, sp, -4
      mv a1, a0
      .while_get_digit:
          lb a0, 0(s3)
          call is_digit
          bltz a0, .end_get_digit
          li t0, 10
          mul a1, a1, t0
          add a1, a1, a0
          addi s3, s3, 1
          j .while_get_digit
      .end_get_digit:
      # pop ra
      lw ra, 4(sp)
      addi sp, sp, 4
      mv a0, a1
      ret

exec_op:
      # push ra
      sw ra, 0(sp)
      addi sp, sp, -4
      la s3, line

    .while_exec:
      mv a0, s3
      call skip_space
      mv s3, a0
      lb t1, 0(s3)

      beqz t1, .zero_exec

      mv a0, t1
      call is_digit
      bgez a0, .digit_op

      xori t0, t1, 43     # if t1 = '+'
      beqz t0, .add_op

      xori t0, t1, 45     # if t1 = '-'
      beqz t0, .sub_op

      xori t0, t1, 42     # if t1 = '*'
      beqz t0, .mul_op

      xori t0, t1, 113    # if t1 = 'q'
      beqz t0, .end

      xori t0, t1, 112    # if t1 = 'q'
      beqz t0, .print_op

      j .continue_exec

    .digit_op:
      addi s3, s3, 1
      call get_digit
      call push_stack
      j .continue_exec

    .print_op:
      call first_stack
      mv a0, a1
      call print_reg
      j .continue_exec

    .sub_op:
      call exec_sub
      j .continue_exec

    .mul_op:
      call exec_mult
      j .continue_exec

    .add_op:
      call exec_add

    .continue_exec:
      addi s3, s3, 1
      j .while_exec
    .zero_exec:
      call get_line
      la s3, line
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

