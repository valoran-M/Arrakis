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

* Fully supported RV32IM instruction set, with most pseudo instructions.
* Different type of environmental calls: Either [Venus environmental calls
  emulation](https://github.com/kvakil/venus/wiki/Environmental-Calls) or
  partial UNIX syscalls.
* A debugging system using breakpoints.

## Installation

Since Arrakis is written in OCaml, it can be installed using
[opam](https://opam.ocaml.org/), OCaml Package Manager.

Simply run:

`$ opam install arrakis`

## Documentation

Documentation about Arrakis usage is available [here](./doc/main.md).

## Development

The `main` branch is currently developped using OCaml 5.0 and Dune 3.10.

For contributions, please take a look at [`CONTRIBUTING.md`](./CONTRIBUTING.md).

## Resources

* [riscv-card](https://github.com/jameslzhu/riscv-card), a comprehensive
  reference sheet for RISC-V.
* [Venus](https://github.com/kvakil/venus), a RISC-V simulator written in
  Kotlin, which inspired Arrakis.
* [CeCILL](http://www.cecill.info/index.en.html), Arrakis License.

