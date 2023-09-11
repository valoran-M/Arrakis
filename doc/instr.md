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


| Inst  | Description            | note                 |
|-------|------------------------|----------------------|
| add   | rd = rs1 + rs2         |                      |
| sub   | rd = rs1 - rs2         |                      |
| xor   | rd = rs1 ^ rs2         |                      |
| or    | rd = rs1 | rs2         |                      |
| and   | rd = rs1 & rs2         |                      |
| sll   | rd = rs1 << rs2        | shift left  logical  |
| srl   | rd = rs1 >> rs2        | shift right logical  |
| sra   | rd = rs1 \>\>a rs2     | shift right arith    |
| slt   | rd = (rs1 \< rs2)?1:0  | signed   comparaison |
| sltu  | rd = (rs1 \<u rs2)?1:0 | unsigned comparaison |
