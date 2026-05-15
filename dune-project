(lang dune 3.17)

(name famicaml)

(generate_opam_files true)

(package
 (name famicaml)
 (synopsis "Project FamiCaml")
 (depends
  (ocaml
   (>= 5.3))
  dune
  cmdliner
  base))
