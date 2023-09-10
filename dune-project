(lang dune 3.10)

(name arrakis)

(using menhir 2.1)
(generate_opam_files true)

(source
 (gitlab valoranM/arrakis))

(authors "Valeran Maytié" "Gurvan Debaussart")

(maintainers "mvalo@proton.me" "gurvan@debauss.art")

(license "CeCILL v2.1")

(documentation https://gitlab.com/valoranM/arrakis/-/blob/main/doc/main.md)

(package
 (name arrakis)
 (synopsis "A RISC-V simulator")
 (description "A longer description")
 (depends ocaml dune alcotest)
 (tags
  (topics "to describe" your project)))
