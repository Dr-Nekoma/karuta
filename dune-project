(lang dune 3.4)
(name karuta)
(license MIT-0)

; https://dune.readthedocs.io/en/stable/opam.html#generating-opam-files
(generate_opam_files true)

(cram enable)

(authors "Lemos" "Magueta" "Marinho")

(maintainers "Lemos" "Magueta" "Marinho")

(source
 (github Dr-Nekoma/karuta))

; https://dune.readthedocs.io/en/stable/concepts.html#package-specification
(package
 (name karuta)
 (synopsis "A compiler for a statically-typed relational programming language")
 (depends ocaml dune menhir batteries)
 (tags
  (prolog logic compiler relational warren)))
