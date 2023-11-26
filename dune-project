(lang dune 3.10)

(name arrakis)

(using menhir 2.1)
(generate_opam_files true)

(source
 (uri https://codeberg.org/Arrakis/arrakis))

(bug_reports https://codeberg.org/Arrakis/arrakis/issues)

(authors "Valeran Maytié" "Gurvan Debaussart")

(maintainers "mvalo@proton.me" "gurvan@debauss.art")

(license "CeCILL-2.1")

(documentation https://codeberg.org/Arrakis/arrakis/wiki)

(package
 (name arrakis)
 (synopsis "A RISC-V simulator")
 (description "A RISC-V simulator  with an integrated debugger.
Support full RV32IM instruction set,
along with pseudo instruction and several environmental calls.")
 (depends (ocaml (>= 4.14.0)) alcotest (menhir (>= "20180528")))
 (tags
  ("RISC-V" "simulator" "emulation" "RV32IM" "education" "debugger" "debug")))
