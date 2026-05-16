(lang dune 3.14)

(generate_opam_files true)

(using menhir 2.1)

(package
 (name gastrolith)
 (synopsis "PL/I card format parser and linter")
 (description "PL/I card format parser and linter with modern warnings")
 (license MIT)
 (maintainers "yoshyhyrro")
 (authors "yoshyhyrro")
 (homepage "https://github.com/yoshyhyrro/gastrolith")
 (bug_reports "https://github.com/yoshyhyrro/gastrolith/issues")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.14))
  str
  angstrom
  ppxlib
  menhir)
 (allow_empty))