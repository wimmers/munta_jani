(lang dune 3.3)

(name munta_jani)

(generate_opam_files true)

(source
 (github wimmers/munta_jani))

(authors "Simon Wimmer")

(maintainers "Simon Wimmer")

(license MIT)

(documentation https://github.com/wimmers/munta_jani)

(package
 (name munta_jani)
 (synopsis "A JANI interface for Munta")
 (description "An interface for checking JANI timed automata models in the
   verified model checker Munta.")
 (depends ocaml dune munta symta)
 (tags
  ("timed automata" "model checking" "verification")))