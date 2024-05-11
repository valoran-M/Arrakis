.data

  .byte 1+2, 2-1, 2*3, 12/3

test:
  .word 1+2, 2-1, 2*3, 12/3, %hi(test), %lo(test), test

