.global _start

_start:
  li a0, 1

  li t0, 20
  li t1, 10
  sub a1, t0, t1
  ecall

  li t0, 0xFFFFFFFF
  li t1, 120
  sub a1, t0, t1
  ecall

/* exit */
  li a0, 10
  ecall

