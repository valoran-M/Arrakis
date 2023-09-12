.data

  hello:  .asciiz "Hello World!\n"

.text

  li a7, 64
  li a0, 1
  la a1, hello
  li a2, 13
  ecall
