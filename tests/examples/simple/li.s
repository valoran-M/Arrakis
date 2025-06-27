.global _start
_start:
  li a0, 1

  li a1, 0b0
  ecall

  li a1, 0b1
  ecall
  li a1, 0b10
  ecall
  li a1, 0b100
  ecall
  li a1, 0b1000
  ecall
  li a1, 0b10000
  ecall
  li a1, 0b100000
  ecall
  li a1, 0b1000000
  ecall
  li a1, 0b10000000
  ecall
  li a1, 0b100000000
  ecall
  li a1, 0b1000000000
  ecall
  li a1, 0b10000000000
  ecall
  li a1, 0b100000000000
  ecall
  li a1, 0b1000000000000
  ecall
  li a1, 0b10000000000000
  ecall
  li a1, 0b100000000000000
  ecall

  li a1, -0b1
  ecall
  li a1, -0b10
  ecall
  li a1, -0b100
  ecall
  li a1, -0b1000
  ecall
  li a1, -0b10000
  ecall
  li a1, -0b100000
  ecall
  li a1, -0b1000000
  ecall
  li a1, -0b10000000
  ecall
  li a1, -0b100000000
  ecall
  li a1, -0b1000000000
  ecall
  li a1, -0b10000000000
  ecall
  li a1, -0b100000000000
  ecall
  li a1, -0b1000000000000
  ecall
  li a1, -0b10000000000000
  ecall
  li a1, -0b100000000000000
  ecall

  li a0, 10
  li a1, 0
  ecall
