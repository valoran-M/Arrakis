# Environmental Calls

## Venus

Arrakis can fully emulate Venus environment calls.
The documentation for those ecall can be found on
[Venus's Wiki](https://github.com/kvakil/venus/wiki/Environmental-Calls)

## UNIX

Arrakis currently support only a small fraction of UNIX system calls, listed
below.

UNIX environment calls are made by loading the ID of the call in the `a7`
registers, and parameters in `a0` to `a6`.

Interesting ressources about system calls in Linux/RISC-V:
* [How to use linux system calls in RISC-V](https://github.com/scotws/RISC-V-tests/blob/master/docs/riscv_linux_system_calls.md)
* [Complete list of system calls ID](https://jborza.com/post/2021-05-11-riscv-linux-syscalls/)

| ID    | Name      |
|-------|-----------|
| 17    | getcwd    |
| 34    | mkdirat   |
| 35    | unlinkat  |
| 37    | link      |
| 49    | chdir     |
| 56    | openat    |
| 57    | close     |
| 63    | read      |
| 64    | write     |
| 93    | exit      |
| 129   | kill      |
| 174   | getuid    |
| 175   | geteuid   |
| 221   | execve    |
| 214   | brk       |
