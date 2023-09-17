# Registers

Registers are storage area inside the CPU.
In RV32 (The specification of RISC-V implemented in Arrakis), each register
can store 32bit of information.

Some registers can be used freely for calculation, while other have special
defined semantics.

A register can either be used with it's name or by using `x[RegisterNumber]`.

| Register number | Name    | Description                           |
|:---------------:|:-------:|:-------------------------------------:|
| 0               | zero    | Hard-Wired zero (Value can't be set)  |
| 1               | ra      | Return adress                         |
| 2               | sp      | Stack pointer                         |
| 3               | gp      | Global pointer                        |
| 4               | tp      | Thread pointer                        |
| 5               | t0      | Temporary register 0                  |
| 6               | t1      | Temporary register 1                  |
| 7               | t2      | Temporary register 2                  |
| 8               | s0/fp   | Temporary register 2                  |
| 9               | s1      | Temporary register 2                  |
| 10              | a0      | Function arg 0 / Return value 0       |
| 11              | a1      | Function arg 1 / Return value 1       |
| 12              | a2      | Function arg 2                        |
| 13              | a3      | Function arg 3                        |
| 14              | a4      | Function arg 4                        |
| 15              | a5      | Function arg 5                        |
| 16              | a6      | Function arg 6                        |
| 17              | a7      | Function arg 7                        |
| 18              | s2      | Saved register 2                      |
| 19              | s3      | Saved register 3                      |
| 20              | s4      | Saved register 4                      |
| 21              | s5      | Saved register 5                      |
| 22              | s6      | Saved register 6                      |
| 23              | s7      | Saved register 7                      |
| 24              | s8      | Saved register 8                      |
| 25              | s9      | Saved register 9                      |
| 26              | s10     | Saved register 10                     |
| 27              | s11     | Saved register 11                     |
| 28              | t3      | Temporary register 3                  |
| 29              | t4      | Temporary register 4                  |
| 30              | t5      | Temporary register 5                  |
| 31              | t6      | Temporary register 6                  |

