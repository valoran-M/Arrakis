# Pseudo-Instructions

Pseudo instructions are not real instruction on Risc-V architecture. They are
compiled to a set of Risc-V instruction.

They are very useful tu simplify assembly code.

## With address

They are used to load all address or to store (s) and load (l) value in
all memory

| Instructions              | Base Instructions                    |
|---------------------------|--------------------------------------|
| la rd, symbol             | auipc rd, symbol[31:12]              |
|                           | addi rd, rd, symbol[11:0]            |
| l{b\|h\|w\|d} rd, symbol  | auipc rd, symbol[31:12]              |
|                           | l{b\|h\|w\|d} rd, symbol\[11:0\] \(rd\) |
| s{b\|h\|w\|d} rd, symbol  | auipc rd, symbol[31:12]              |
|                           | s{b\|h\|w\|d} rd, symbol\[11:0\] \(rd\) |

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


Li instruction :

    It used to load every 32 bites immediate in a register

Compilation :

```
li rd, imm ->

    if imm[31:12] = 0 then
        addi rd, x0, imm
    else
        lui rd, (imm[31:12] >> 12)
        addi rd, x0, imm[11:0]
```
