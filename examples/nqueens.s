# nqueens ----------------------------------------------------------------------
# Returns the number of solution for the nqueens problem
#
# This file is part of Arrakis <https://codeberg.org/arrakis/arrakis>
# It is distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>
# ------------------------------------------------------------------------------

.text

t.part.0:
  addi    sp, sp, -32
  sw      s1, 20(sp)
  not     s1, a2
  and     s1, s1, a0
  not     a5, a1
  and     s1, s1, a5
  sw      s0, 24(sp)
  neg     s0, s1
  sw      s2, 16(sp)
  sw      ra, 28(sp)
  and     s0, s0, s1
  li      s2, 0
  beq     s0, zero, .L1
  sw      s3, 12(sp)
  sw      s4, 8(sp)
  sw      s5, 4(sp)
  mv      s3, a0
  mv      s5, a2
  mv      s4, a1
  li      s2, 0
.L4:
  add     a5, s5, s0
  srli    a2, a5, 31
  add     a2, a2, a5
  add     a1, s4, s0
  sub     a0, s3, s0
  srai    a2, a2, 1
  slli    a1, a1, 1
  li      a5, 1
  beq     a0, zero, .L3
  call    t.part.0
  mv      a5, a0
.L3:
  sub     a4, s1, s0
  sub     s0, s0, s1
  and     s0, a4, s0
  add     s2, s2, a5
  beq     s0, zero, .L11
  mv      s1, a4
  j       .L4
.L11:
  lw      s3, 12(sp)
  lw      s4, 8(sp)
  lw      s5, 4(sp)
.L1:
  lw      ra, 28(sp)
  lw      s0, 24(sp)
  lw      s1, 20(sp)
  mv      a0, s2
  lw      s2, 16(sp)
  addi    sp, sp,32
  jr      ra
t:
  beq     a0, zero, .L13
  tail    t.part.0
.L13:
  li      a0, 1
  ret
.globl _start
_start:
  li      a5, -1
  li      a0, 2
  sll     a0, a5, a0
  not     a0, a0
  beq     a0, zero, .L15
  li      a2, 0
  li      a1, 0
  tail    t.part.0
.L15:
  li      a0,1
  ret
