(lang dune 3.13)

(name ann-print-demo)

(generate_opam_files true)

(package
 (name ann-print-demo)
 (maintainers "Mathis Bouverot-Dupuis (mathis.bouverot@ens.psl.eu)")
 (authors "Mathis Bouverot-Dupuis")
 (synopsis "A pretty-printing library.")
 (depends
  (tyxml
   (>= 4.6.0))))
