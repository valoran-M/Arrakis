# Instructions


Most instructions are three-address codded

This means we have 3 parameters :

```
    inst d, p1, p2

d  : destiniation
p1 : first parameter
p2 : second parameter
```

Risc-V is composed of 6 instructions types :

* R type
* I type
* S type
* B type
* U type
* J type


Warning :

    The coding of I, S, B instructions implies that the immediate is coded on 12 bits,
    there are pseudo instructions to avoid some mistake.

## R type

Format :
```as
    inst rd, rs1, rs2
```

### RV32I

| Inst  | Description            | note                 |
|-------|------------------------|----------------------|
| add   | rd = rs1 + rs2         |                      |
| sub   | rd = rs1 - rs2         |                      |
| xor   | rd = rs1 ^ rs2         |                      |
| or    | rd = rs1 \| rs2        |                      |
| and   | rd = rs1 & rs2         |                      |
| sll   | rd = rs1 << rs2        | shift left  logical  |
| srl   | rd = rs1 >> rs2        | shift right logical  |
| sra   | rd = rs1 \>\>a rs2     | shift right arith    |
| slt   | rd = (rs1 \< rs2)?1:0  | signed   comparaison |
| sltu  | rd = (rs1 \<u rs2)?1:0 | unsigned comparaison |

### RV32M Multiply Extension


| Inst  | Description            | note                 |
|-------|------------------------|----------------------|
| mul   | rd = (rs1 * rs2)[31:0] |                      |
| mulh  | rd = (rs1 * rs2)[62:32]|                      |
| mulhsu| rd = (rs1 * rs2)[62:32]|                      |
| mulu  | rd = (rs1 * rs2)[62:32]|                      |
| div   | rd = rs1 / rs2         |                      |
| divu  | rd = rs1 /u rs2        | |
| rem   | rd = rs1 % rs2         | |
| remu  | rd = rs1 %u rs2        | |

## I type

Warning :

    The coding of instructions implies that the immediate is coded on 12 bits,
    there are pseudo instructions to avoid some mistake.

### Normal

Format :
```as
    inst rd, rs1, imm
```

| Inst  | Description            | note                 |
|-------|------------------------|----------------------|
| add   | rd = rs1 + imm         |                      |
| sub   | rd = rs1 - imm         |                      |
| xor   | rd = rs1 ^ imm         |                      |
| or    | rd = rs1 \| imm        |                      |
| and   | rd = rs1 & imm         |                      |
| sll   | rd = rs1 << imm[0:4]   | shift left  logical  |
| srl   | rd = rs1 >> imm[0:4]   | shift right logical  |
| sra   | rd = rs1 \>\>a imm[0:4]| shift right arith    |
| slt   | rd = (rs1 \< imm)?1:0  | signed   comparaison |
| sltu  | rd = (rs1 \<u imm)?1:0 | unsigned comparaison |
|-------|------------------------|----------------------|
| jalr  | rd = PC+4; PC+=rs1+imm | Jump And Link        |

### Load

Format :
```as
    inst rd, imm(rs1)
```

| Inst  | Description            | note                 |
|-------|------------------------|----------------------|
| lb    | rd = M\[rs1+imm\][0:7] | load byte            |
| lh    | rd = M\[rs1+imm\][0:15]| load half            |
| lw    | rd = M\[rs1+imm\][0:31]| load word            |
| lbu   | rd = M\[rs1+imm\][0:7] | load byte unsigned   |
| lhu   | rd = M\[rs1+imm\][0:15]| load half unsigned   |

### Other

Finally, there is the `ecall` instruction, which is used to make
[system calls](./ecall.md)

## S type

Format :
```as
    inst rs2, imm(rs1)
```

| Inst  | Description            | note                 |
|-------|------------------------|----------------------|
| sb    | M\[rs1+imm\]=rs2[0:7]  | store byte           |
| sh    | M\[rs1+imm\]=rs2[0:15] | store half           |
| sw    | M\[rs1+imm\]=rs2[0:31] | store word           |

