(lang dune 3.20)

(name famicaml)

(generate_opam_files true)

(package
 (name famicaml)
 (synopsis "Project FamiCaml")
(depends
 (ocaml (>= 5.3))
 dune
 cmdliner
 base
 stdint
 js_of_ocaml
 js_of_ocaml-ppx))
