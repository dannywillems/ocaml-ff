(lang dune 3.15)

(name ff)

(generate_opam_files true)

(license MIT)

(authors "Danny Willems <be.danny.willems@gmail.com>")

(maintainers "Danny Willems <be.danny.willems@gmail.com>")

(source
 (github dannywillems/ocaml-ff))

(homepage "https://github.com/dannywillems/ocaml-ff")

(bug_reports "https://github.com/dannywillems/ocaml-ff/issues")

(package
 (name ff-sig)
 (synopsis "Minimal finite field signatures")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.15))
  (zarith
   (and
    (>= 1.9.1)
    (< 2.0)))
))

(package
 (name ff)
 (synopsis "OCaml implementation of Finite Field operations")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.15))
  (zarith
   (and
    (>= 1.9.1)
    (< 2.0)))
  (ff-sig
   (= :version))
  (alcotest :with-test)
  (ff-pbt
   (and
    (= :version)
    :with-test))
))

(package
 (name ff-pbt)
 (synopsis
  "Property based testing library for finite fields over the package ff-sig")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.15))
  (zarith
   (and
    (>= 1.9.1)
    (< 2.0)))
  (ff-sig
   (= :version))
  alcotest
))
