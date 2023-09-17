# Compiling

After successfully understanding how to write assembly code and testing it with
Arrakis, you may wish to see how it can be used in a real system.

If you are not on a RISC-V machine, you need to use
[cross compilation](https://en.wikipedia.org/wiki/Cross_compiler), along with a
way to emulate the target machine.

One emulator that can be used is [qemu](https://www.qemu.org/).
For compiling, you can use
[gnu binutils](https://www.gnu.org/software/binutils/). Note that you must use
a gnu binutils compiled for the RISCV32 target.

## Installing software

### Arch

`# pacman -S riscv32-elf-binutils qemu-user`

## Compiling and emulation

There is three steps to fully emulate how your program will behave on a real
system:
1. [Assembling](#assembling)
2. [Linking](#linking)
3. [Emulating](#emulating)

We will take as an example the file [`helloworld.s`](../examples/helloworld.s).

### Assembling

Assembling can be done with the GNU assembler,
[`as`](https://ftp.gnu.org/old-gnu/Manuals/gas-2.9.1/html_node/as_toc.html#TOC3).

`$ riscv32-elf-as helloworld.s -o helloworld.o`

### Linking

Linking can be done with the GNU linker,
[`ld`](https://ftp.gnu.org/old-gnu/Manuals/ld-2.9.1/html_mono/ld.html).

`$ riscv32-elf-ld helloworld.o -o helloworld`

### Emulating

Emulating the system can be done with [qemu](https://www.qemu.org/),
particularily with the qemu user-mode emulation.

Simply run:
`$ qemu-riscv32 ./helloworld`
