(lang dune 2.0)
(name satex)
(version 0.1.1)
(source (github smimram/satex))
(license GPL-3.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
(using menhir 2.0)

(generate_opam_files true)

(package
 (name satex)
 (synopsis "String diagram generator for LaTeX")
 (description "Satex takes descriptions of string diagrams in the form of categorical compositions and converts them to string diagrams drawn in TikZ.")
 (depends
  (menhir :build)
 )
)
