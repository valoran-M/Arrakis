(lang dune 3.10)

(name arrakis)

(using menhir 2.1)
(generate_opam_files true)

(source
 (gitlab valoranM/arrakis))

(authors "Valeran Maytié" "Gurvan Debaussart")

(maintainers "valeran.maytie@universite-paris-saclay.fr" "gurvan@debauss.art")

(license "CeCILL v2.1")

(documentation https://url/to/documentation)

(package
 (name arrakis)
 (synopsis "A RISC-V simulator")
 (description "A longer description")
 (depends ocaml dune alcotest)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
