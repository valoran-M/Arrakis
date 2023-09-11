# Instructions

Most instructions are three-address codded, which mean they take three
parameters:

```
    inst d, p1, p2

d  : destiniation
p1 : first parameter
p2 : second parameter
```

RISC-V is composed of 6 instructions types:

* [R type](#R-type)
* [I type](#I-type)
* [S type](#S-type)
* [B type](#B-type)
* [U type](#U-type)
* [J type](#J-type)

Warning:

    The coding of I, S, B instructions implies that the immediate is coded on 12 bits,
    there are pseudo instructions to avoid some mistake.

## R type

Format:
```as
    inst rd, rs1, rs2
```

### RV32I

| Inst  | Description            | Note                 |
|-------|------------------------|----------------------|
| add   | rd = rs1 + rs2         |                      |
| sub   | rd = rs1 - rs2         |                      |
| xor   | rd = rs1 ^ rs2         |                      |
| or    | rd = rs1 \| rs2        |                      |
| and   | rd = rs1 & rs2         |                      |
| sll   | rd = rs1 << rs2        | Shift left  logical  |
| srl   | rd = rs1 >> rs2        | Shift right logical  |
| sra   | rd = rs1 \>\>a rs2     | Shift right arith    |
| slt   | rd = (rs1 \< rs2)?1:0  | Signed   comparaison |
| sltu  | rd = (rs1 \<u rs2)?1:0 | Unsigned comparaison |

### RV32M (Multiply Extension)

| Inst  | Description            | Note                 |
|-------|------------------------|----------------------|
| mul   | rd = (rs1 * rs2)[31:0] |                      |
| mulh  | rd = (rs1 * rs2)[62:32]|                      |
| mulhsu| rd = (rs1 * rs2)[62:32]|                      |
| mulu  | rd = (rs1 * rs2)[62:32]|                      |
| div   | rd = rs1 / rs2         |                      |
| divu  | rd = rs1 /u rs2        |                      |
| rem   | rd = rs1 % rs2         |                      |
| remu  | rd = rs1 %u rs2        |                      |

## I type

Warning:

    The coding of instructions implies that the immediate is coded on 12 bits,
    there are pseudo instructions to avoid some mistake.

### Normal

Format:
```as
    inst rd, rs1, imm
```

| Inst  | Description            | Note                 |
|-------|------------------------|----------------------|
| add   | rd = rs1 + imm         |                      |
| sub   | rd = rs1 - imm         |                      |
| xor   | rd = rs1 ^ imm         |                      |
| or    | rd = rs1 \| imm        |                      |
| and   | rd = rs1 & imm         |                      |
| sll   | rd = rs1 << imm[0:4]   | Shift left  logical  |
| srl   | rd = rs1 >> imm[0:4]   | Shift right logical  |
| sra   | rd = rs1 \>\>a imm[0:4]| Shift right arith    |
| slt   | rd = (rs1 \< imm)?1:0  | Signed   comparaison |
| sltu  | rd = (rs1 \<u imm)?1:0 | Unsigned comparaison |
|-------|------------------------|----------------------|
| jalr  | rd = PC+4; PC+=rs1+imm | Jump And Link        |

### Load

Format :
```as
    inst rd, imm(rs1)
```

| Inst  | Description            | Note                 |
|-------|------------------------|----------------------|
| lb    | rd = M\[rs1+imm\][0:7] | Load byte            |
| lh    | rd = M\[rs1+imm\][0:15]| Load half            |
| lw    | rd = M\[rs1+imm\][0:31]| Load word            |
| lbu   | rd = M\[rs1+imm\][0:7] | Load byte unsigned   |
| lhu   | rd = M\[rs1+imm\][0:15]| Load half unsigned   |

### Other

The last I type instruction is `ecall`, which is used to make
[environmental calls](./ecall.md).

## S type

Format:
```as
    inst rs2, imm(rs1)
```

| Inst  | Description            | Note                 |
|-------|------------------------|----------------------|
| sb    | M\[rs1+imm\]=rs2[0:7]  | Store byte           |
| sh    | M\[rs1+imm\]=rs2[0:15] | Store half           |
| sw    | M\[rs1+imm\]=rs2[0:31] | Store word           |

## B type

Format:
```as
    inst rs2, rs1, imm
```

| Inst  | Description              | Note                 |
|-------|--------------------------|----------------------|
| beq   | if(rs1 == rs2) PC += imm |                      |
| bne   | if(rs1 != rs2) PC += imm |                      |
| blt   | if(rs1 <  rs2) PC += imm |                      |
| bge   | if(rs1 >= rs2) PC += imm |                      |
| bltu  | if(rs1 <  rs2) PC += imm |                      |
| bgeu  | if(rs1 >= rs2) PC += imm |                      |

## U type

