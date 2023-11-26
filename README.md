<div align="center">
<h1>
    Arrakis
    <br>
    <img src="./imgs/logo.png" width="200" />
    </br>
</h1>
</div>

[Arrakis](https://en.wikipedia.org/wiki/Arrakis) is a RISC-V simulator written
in OCaml, primarly designed for education.

## Features

* Fully supported RV32IM instruction set, with [most pseudo instructions](https://codeberg.org/Arrakis/arrakis/wiki/RV_Pseudo-Instructions).
* Different type of environmental calls: either [UNIX syscalls](https://codeberg.org/Arrakis/arrakis/wiki/RV_Environment-call)
  (default) or
  [Venus environmental calls emulation](https://codeberg.org/Arrakis/arrakis/wiki/RV_Environment-call).
* Partial GNU `as` [assembler directives](https://codeberg.org/Arrakis/arrakis/wiki/RV_Assembler-Directives).
* A debugging system using breakpoints.
* Complete [documentation](https://codeberg.org/Arrakis/arrakis/wiki),
  covering not only Arrakis usage but also serving as a general guide on how to
  write RISC-V assembly code.
* (WIP) A [vim plugin](https://codeberg.org/Arrakis/arrakis.vim) to integrate
  Arrakis with your favorite text editor.

## Installation

Since Arrakis is written in OCaml, it can be installed using
[opam](https://opam.ocaml.org/), OCaml Package Manager.

Simply run:

`$ opam install arrakis`

## Documentation

Documentation about Arrakis usage is available
[here](https://codeberg.org/Arrakis/arrakis/wiki)

## Development

The `main` branch is currently developped using OCaml 5.0 and Dune 3.10.

For contributions, please take a look at [`CONTRIBUTING.md`](./CONTRIBUTING.md).

## Resources

* [riscv-card](https://github.com/jameslzhu/riscv-card), a comprehensive
  reference sheet for RISC-V.
* [Venus](https://github.com/kvakil/venus), a RISC-V simulator written in
  Kotlin, which inspired Arrakis.
* [CeCILL](http://www.cecill.info/index.en.html), Arrakis License.

