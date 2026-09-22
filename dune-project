(lang dune 3.24)
(using macocaml 0.1)

(name fast_gen)

(generate_opam_files true)

(version 0.1.0) 

(authors "Joseph Cutler <jwc@seas.upenn.edu>" "Thia Richey <lapwing@seas.upenn.edu>")

(maintainers "Maintainer Name <maintainer@example.com>")

(package
 (name fast_gen)
 (synopsis "Staged property-based-testing generators (AllegrOCaml), MacoCaml port")
 (description "MacoCaml port of the AllegrOCaml staged PBT generator library")
 (depends
  ocaml
  (alcotest :with-test)
  core
  base_quickcheck
 ))
