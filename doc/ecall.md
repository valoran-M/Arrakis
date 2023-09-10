# Environmental Calls

## Using ecall

Environmental calls are made with the `ecall` instruction.
To use a specific `ecall`, load it's id into the `a0` register and parameters in
the `a1` to `a7` registers.

## Venus

Arrakis can fully emulate Venus environment calls. The documentation for those
ecall can be found on
[Venus's Wiki](https://github.com/kvakil/venus/wiki/Environmental-Calls)

## Unix

Arrakis currently support only a small fraction of UNIX system calls.
More information about usage of linux syscall in risc-v can be found
[here](https://jborza.com/post/2021-05-11-riscv-linux-syscalls/).

| ID    | Name      |
|-------|-----------|
| 17    | getcwd    |
| 34    | mkdirat   |
| 35    | unlinkat  |
| 37    | link      |
| 49    | chdir     |
| 56    | open      |
| 57    | close     |
| 63    | read      |
| 64    | write     |
| 93    | exit      |
| 129   | kill      |
| 174   | getuid    |
| 175   | geteuid   |
| 221   | execve    |
| 214   | brk       |
