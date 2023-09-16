# Pseudo-Instructions

Pseudo instructions are not real instruction on RISC-V architecture.
They are compiled to a set of RISC-V instruction.

They are very useful tu simplify assembly code.

## With address

They are used to load all address or to store (s) and load (l) value in
all memory

| Instructions              | Base Instructions                                |
|---------------------------|--------------------------------------------------|
| la rd, symbol             | auipc rd, symbol[31:12]                          |
|                           | addi rd, rd, symbol[11:0]                        |
| l{b\|h\|w\|d} rd, symbol  | auipc rd, symbol[31:12]                          |
|                           | l{b\|h\|w\|d} rd, symbol\[11:0\] \(rd\)          |
| s{b\|h\|w\|d} rd, symbol  | auipc rd, symbol[31:12]                          |
|                           | s{b\|h\|w\|d} rd, symbol\[11:0\] \(rd\)          |

## Arithmetic

| Instruction   | Base Instructions |
|---------------|-------------------|
| nop           | addi x0, x0, 0    |
| mv rd, rs     | addi rd, rs, 0    |
| not rd, rs    | xori rd, rs, -1   |
| neg rd, rs    | sub rd, x0, rs    |
| seqz rd, rs   | sltiu rd, rs, 1   |
| snez rd, rs   | sltu rd, x0, rs   |
| sltz rd, rs   | slt rd, rs, x0    |
| sgtz rd, rs   | slt rd, x0, rs    |


li instruction :

    Used to load every 32 bites immediate in a register.

Compilation :

```
li rd, imm ->

    if imm[31:12] = 0 then
        addi rd, x0, imm
    else
        lui rd, (imm[31:12] >> 12)
        addi rd, x0, imm[11:0]
```

## Branch

| Instruction      | Base Instructions  | Description |
|------------------|--------------------|-------------|
| beqz rs, offset  | beq rs, x0, offset | if == zero  |
| bnez rs, offset  | bne rs, x0, offset | if != zero  |
| blez rs, offset  | bge x0, rs, offset | if <= zero  |
| bgez rs, offset  | bge rs, x0, offset | if >= zero  |
| bltz rs, offset  | blt rs, x0, offset | if <  zero  |
| bgtz rs, offset  | blt x0, rs, offset | if >  zero  |

## Jump

| Instruction      | Base Instructions        | Description                 |
|------------------|--------------------------|-----------------------------|
| j offset         | jal x0, offset           | Jump                        |
| jal offset       | jal x1, offet            | Jump and link               |
| jr rs            | jalr x0, rs, 0           | Jump register               |
| jalr rs          | jalr x1, rs, 0           | Jump and link register      |
| ret              | jalr x0, x1, 0           | Return from subroutine      |
| call offset      | auipc x1, offset[31:12]  | Call far-away subroutine    |
|                  | jalr x1, x1, offet[11:0] |                             |
| tail offset      | auipc x6, offset[31:12]  | Tail call far-away          |
|                  | jalr x0, x6, offset[11:0]|                             |

