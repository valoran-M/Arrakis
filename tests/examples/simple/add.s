.global _start

_start:
  li a0, 1

  li t0, 20
  li t1, 10
  add a1, t0, t1
  ecall

  li t0, 120
  li t1, 0xFFFFFFFF
  add a1, t0, t1
  ecall

/* exit */
  li a0, 10
  ecall


