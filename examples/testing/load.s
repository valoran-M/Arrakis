.data

  list:   .byte 0x10, 0x12
          .word 0x13, 0x14
  hello:  .asciz "Hello World!\n"

.text
  lb t0, list
  lh t0, list
  addi sp, sp, -0xF

  la a1, hello
  addi a0, x0, 4
  bgt x0, x0, .test
.test:
  ecall
