(lang dune 3.3)

(name ocaml_piet)

(generate_opam_files true)

(source
 (github adallon/ocaml-piet))

(authors "A. Dallon")

(maintainers "A. Dallon")

(license CeCILL-C www.cecill.info)

(documentation lib/)

(package
 (name ocaml_piet)
 (synopsis "Piet interpreter")
 (description "OCaml piet esoteric programming language implementation")
 (depends ocaml dune imagelib odoc)
 (documentation "https://github.com/adallon/ocaml-piet")
 (tags
  (topics piet ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
