.data

  stack_size: .word 1024
  stack:      .zero 1024

  line_size: .word 256
  line:      .zero 256

  print:     .zero 5
             .asciz "\n"

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
  addi a1, a1, 20
  li a2, 1
  li t1, 10
  .while_print_reg:
    addi a1, a1, -1
    rem t2, a0, t1
    addi t2, t2, '0'
    sb t2, 0(a1)
    div a0, a0, t1
    addi a2, a2, 1
    bnez a0, .while_print_reg
  li a7, 64
  li a0, 1
  ecall
  ret
.size print_reg, .-print_reg

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
.size is_digit, .-is_digit

# Stack ########################################################################

# Initialise stack
init_stack:
  la s1, stack
  li s2, 0
  ret
.size init_stack, .-init_stack

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
.size push_stack, .-push_stack

# Pop first argument of the stack
#
# return:
# a0 : 1 if succeed else 0
# a1 : pop number
pop_stack:
    blez s2, .stack_empty_pop
    lw a1, -4(s1)
    addi s1, s1, -4
    addi s2, s2, -4
    li a0, 1
    ret
.stack_empty_pop:
    li a0, 0
    ret
.size pop_stack, .-pop_stack

# Get first element of the stack
#
# return:
# a0 : 1 if succeed else 0
# a1 : first argument
first_stack:
  blez s2, .stack_empty_first
  lw a1, -4(s1)
  li a0, 1
  ret
.stack_empty_first:
  li a0, 0
  ret
.size first_stack, .-first_stack

# Operator #####################################################################

exec_add:
  # push ra
  addi sp, sp, -4
  sw ra, 0(sp)

  call pop_stack
  mv t0, a1
  call pop_stack
  add a0, t0, a1
  call push_stack

  # pop ra
  lw ra, 0(sp)
  addi sp, sp, 4
  ret
.size exec_add, .-exec_add

exec_sub:
  # push ra
  addi sp, sp, -4
  sw ra, 0(sp)

  call pop_stack
  mv t0, a1
  call pop_stack
  sub a0, a1, t0
  call push_stack

  # pop ra
  lw ra, 0(sp)
  addi sp, sp, 4
  ret
.size exec_sub, .-exec_sub

exec_mult:
  # push ra
  addi sp, sp, -4
  sw ra, 0(sp)

  call pop_stack
  mv t0, a1
  call pop_stack
  mul a0, t0, a1
  call push_stack

  # pop ra
  lw ra, 0(sp)
  addi sp, sp, 4
  ret
.size exec_mult, .-exec_mult

# input ########################################################################

# Skip empty character
# a0 : string address
#
# return:
# a0 : new string pointer
skip_space:
.while_skip_space:
  lb t0, 0(a0)
  xori t1, t0, '\t'    # 0 if t1 = '\t'
  beqz t1, .lazy_or
  xori t0, t0, ' '   # 0 if t1 = ' '
  bnez t0, .end_skip_space
.lazy_or:
  addi a0, a0, 1
  j .while_skip_space
.end_skip_space:
  ret
.size skip_space, .-skip_space

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
.size get_line, .-get_line

# Read line for get an integer read on (s3) string
# a0 : first digit
#
# return:
# a0 : digit
get_digit:
  # push ra
  addi sp, sp, -4
  sw ra, 0(sp)
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
  lw ra, 0(sp)
  addi sp, sp, 4
  mv a0, a1
  ret
.size get_digit, .-get_digit

exec_op:
  # push ra
  addi sp, sp, -4
  sw ra, 0(sp)
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

  xori t0, t1, '+'      # if t1 = '+'
  beqz t0, .add_op

  xori t0, t1, '-'      # if t1 = '-'
  beqz t0, .sub_op

  xori t0, t1, '*'      # if t1 = '*'
  beqz t0, .mul_op

  xori t0, t1, 'q'      # if t1 = 'q'
  beqz t0, .end

  xori t0, t1, 'p'      # if t1 = 'p'
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
  lw ra, 0(sp)
  addi sp, sp, 4
  ret
.size exec_op, .-exec_op

# Main #########################################################################

.globl _start
_start:
  call init_stack

  call exec_op

  # exit
  li a7, 93
  li a0, 0
  ecall
.size _start, .-_start

