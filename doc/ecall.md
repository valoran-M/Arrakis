# Environmental Calls

Environmental calls are made with the `ecall` instruction.
As their name suggest, the actual semantics of the instruction depends on the
environment.

Arrakis currently implement two kind of environment: [UNIX](#unix) and
[Venus](#venus).

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

### Examples

* [Hello, World!](../examples/helloworld.s)
 Write a simple 'Hello, World!' to stdout, and then exit.

* [Hello, [name]](../examples/helloname.s)
 Read a name from stdin, write 'Hello, [name]!' in stdout, and then exit with
 a proper syscall.

## Venus

The documentation for Venus ecall can be found on
[Venus's Wiki](https://github.com/kvakil/venus/wiki/Environmental-Calls)

