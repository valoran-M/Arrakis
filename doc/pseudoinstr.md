# Pseudo-Instructions

Pseudo instructions are not real instruction on Risc-V architecture. They are
compiled to a set of Risc-V instruction.

They are very usfull tu simplify assembly code.

## With address

| instruction   | Format                    | Base Instrctions                     |
|---------------|---------------------------|--------------------------------------|
| la            | la rd, symbol             | auipc rd, symbol[31:12]              |
|               |                           | addi rd, rd, symbol[11:0]            |
| l{b\|h\|w\|d} | l{b\|h\|w\|d} rd, symbol  | auipc rd, symbol[31:12]              |
|               |                           | l{b\|h\|w\|d} rd, symbol\[11:0\](rd) |
| s{b\|h\|w\|d} | s{b\|h\|w\|d} rd, symbol  | auipc rd, symbol[31:12]              |
|               |                           | s{b\|h\|w\|d} rd, symbol\[11:0\](rd) |
