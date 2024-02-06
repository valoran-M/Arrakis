      addi x1, x0, 10
      bne  x0, x0, .A
      addi x1, x0, -10
      beq x1, x1, .A
      addi x2, x0, 20
.A:   addi x2, x0, 30
