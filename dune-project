(lang dune 3.17)

(name may)

; (generate_opam_files true)

(generate_opam_files)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name may)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ; (ocaml (< 5.3.0))
  ocaml
  base
  ; base_composition_infix
  expectable
  vec
  core
  core_unix
  async
  menhir
  textutils
  ppx_jane
  bisect_ppx)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
