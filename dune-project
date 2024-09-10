(lang dune 3.16)

(using melange 0.1)

(name rbtree)

(generate_opam_files true)

(source
 (github andrewbaine/rbtree))

(maintainers "Andrew Baine <andrew.baine@gmail.com>")

(authors "Andrew Baine <andrew.baine@gmail.com>")

(license "MIT")

(package
 (name rbtree)
 (authors "Andrew Baine")
 (synopsis "Functional Red-Black Tree")
 (description "Functional Red-Black Tree in Ocaml")
 (depends
  (ocaml
   (>= 4.14.0))
  (melange
   (>= "4.0.0"))
  (alcotest :with-test)
  (core :with-test)
  (melange-jest :with-test)
  (merlin :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))
