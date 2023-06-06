(lang dune 2.7)

(implicit_transitive_deps false)

(formatting
 (enabled_for dune))

(name mlpost)

(license LGPL-2.1)

(authors
 "Romain Bardou"
 "François Bobot"
 "Jean-Christophe Filliâtre <filliatr@lri.fr>"
 "Johannes Kanig"
 "Stephane Lescuyer"
 "Claude Marché"
 "Florence Plateau")

(maintainers "Jean-Christophe Filliâtre <filliatr@lri.fr>")

(source
 (github backtracking/mlpost))

(generate_opam_files true)

(package
 (name mlpost)
 (synopsis "OCaml library on top of Metapost")
 (description
  "OCaml toolchain to work with WebAssembly, including and interpreter")
 (tags
  (ocaml latex figure mlpost metapost graphics))
 (depends
  (ocaml
   (>= 4.07))
  ocamlfind
  (bitstring
   (>= 3.0.0))
  ppx_bitstring
  (cairo2
   (>= 0.6.2))
  (ppxlib
   (>= 0.20.0))
  (odoc :with-doc)
  (cmdliner (>= 1.1.0))
 )
 (depopts graphics))

(package
 (name mlpost-lablgtk)
 (synopsis "Library for adding mlpost graphics in lablgtk")
 (description "Library for adding mlpost graphics in lablgtk")
 (tags
  (ocaml latex figure mlpost metapost graphics lablgtk gtk))
 (depends
  (ocaml
   (>= 4.07))
  (bitstring
   (>= 3.0.0))
  cairo2-gtk
  (mlpost :version)
  lablgtk
  conf-gnomecanvas
  (odoc :with-doc)))

(cram enable)
