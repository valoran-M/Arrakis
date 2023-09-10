# Environmental Calls

## Using ecall

Environmental calls are made with the `ecall` instruction.
To use a specific `ecall`, load it's id into `a0` and parameters in `a1` to
`a7`.

When "pointed out" is used in the documentation, it means that the register
should contain a pointer to the memory.

## Venus

Arrakis can fully emulate Venus environment calls.
The documentation for those ecall can be found on
[Venus's Wiki](https://github.com/kvakil/venus/wiki/Environmental-Calls)

## Linux

Arrakis currently support only a small fraction of UNIX system calls, listed
below.
More information about Linux system calls and how to use them in RISC-V can be
found [here](https://jborza.com/post/2021-05-11-riscv-linux-syscalls/).


| ID    | Name                    |
|-------|-------------------------|
| 17    | [getcwd](#getcwd)     |
| 34    | [mkdirat](#mkdirat)   |
| 35    | [unlinkat](#unlinkat) |
| 37    | [link](#link)         |
| 49    | [chdir](#chdir)       |
| 56    | [open](#open)         |
| 57    | [close](#close)       |
| 63    | [read](#read)         |
| 64    | [write](#write)       |
| 93    | [exit](#exit)         |
| 129   | [kill](#kill)         |
| 174   | [getuid](#getuid)     |
| 175   | [geteuid](#geteuid)   |
| 221   | [execve](#execve)     |
| 214   | [brk](#brk)           |

### getcwd

Write current working directory to memory pointed at by `a1`.

### mkdirat

### unlinkat

### link

### chdir

### open

### close

### read

### write

### exit

### kill

Send signal in `a2` to process with pid `a1`.

### getuid

Write uid (User id) in `a0`.

### geteuid

Write euid (Effective user id) in `a0`.

### execve

Execute program with pathname pointed at by `a1` with arguments specified
by `a2` to `a7`.

### brk

