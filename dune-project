(lang dune 3.0)

(name containers)

(generate_opam_files true)

(version 3.17)

(authors c-cube)

(maintainers c-cube)

(license BSD-2-Clause)

(homepage "https://github.com/c-cube/ocaml-containers/")

(source
 (github c-cube/ocaml-containers))

(package
 (name containers)
 (synopsis
  "A modular, clean and powerful extension of the OCaml standard library")
 (tags
  (stdlib containers iterators list heap queue))
 (depends
  (ocaml
   (>= 4.08))
  either
  dune-configurator
  (qcheck-core
   (and
    (>= 0.91)
    :with-test))
  (yojson :with-test)
  (iter :with-test)
  (gen :with-test)
  (csexp :with-test)
  (uutf :with-test)
  (odoc :with-doc))
 (depopts base-unix base-threads))

(package
 (name containers-data)
 (synopsis "A set of advanced datatypes for containers")
 (tags
  (containers RAL function vector okasaki))
 (depends
  (ocaml
   (>= 4.08))
  (containers
   (= :version))
  (qcheck-core
   (and
    (>= 0.91)
    :with-test))
  (iter :with-test)
  (gen :with-test)
  (mdx :with-test)
  (odoc :with-doc)))
