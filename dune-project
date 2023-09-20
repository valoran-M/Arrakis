(lang dune 3.10)

(name arrakis)

(using menhir 2.1)
(generate_opam_files true)

(source
 (gitlab valoranM/arrakis))

(authors "Valeran Maytié" "Gurvan Debaussart")

(maintainers "mvalo@proton.me" "gurvan@debauss.art")

(license "CeCILL-2.1")

(documentation https://gitlab.com/valoranM/arrakis/-/wikis/)

(package
 (name arrakis)
 (synopsis "A RISC-V simulator")
 (description "A RISC-V simulator  with an integrated debugger.
Support full RV32IM instruction set,
along with pseudo instruction and several environmental calls.")
 (depends (ocaml (>= 4.14.0)) alcotest)
 (tags
  ("RISC-V" "simulator" "emulation" "RV32IM" "education" "debugger" "debug")))
