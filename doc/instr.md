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

