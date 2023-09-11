.data
  0x10 0x34

  list:   .byte 0x10 0x12
          .word 0x13 0x14
  hello:  .asciiz "Hello World!\n"

.text
  lb t0, list
  lh t0, list
  li t0, 10000
#  addi sp, sp, -0xF
#  addi t0, x0, 0x10
#
#  sb t0, 0xF(sp)

  la a1, hello
