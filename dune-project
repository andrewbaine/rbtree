(lang dune 3.11)

(name rbtree)

(generate_opam_files true)

(source
 (github andrewbaine/rbtree))

(authors "Andrew Baine")

(maintainers "Andrew Baine")

(license MIT)

(documentation https://github.com/andrewbaine/rbtree)

(package
 (name rbtree)
 (synopsis "Functional Red-Black Tree")
 (description "Functional Red-Black Tree in Ocaml")
 (depends ocaml dune core (alcotest :with-test))
 (tags
  ("red-black" functional tree "data structures")))
 
