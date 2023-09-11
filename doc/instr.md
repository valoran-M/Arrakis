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


| Inst  | Description |
|-------|-------------|
| add   | rd = rs1 + rs2         |
| sub   | rd = rs1 - rs2         |
| xor   | rd = rs1 ^ rs2         |
| or    | rd = rs1 | rs2         |
| and   | rd = rs1 & rs2         |
| sll   | rd = rs1 << rs2        |
| srl   | rd = rs1 >> rs2        |
| sra   | rd = rs1 \>\>a rs2     |
| slt   | rd = (rs1 \< rs2)?1:0  |
| sltu  | rd = (rs1 \<u rs2)?1:0 |
