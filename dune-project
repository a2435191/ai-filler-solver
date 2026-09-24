(lang dune 3.24)

(name filler)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name filler)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (= 5.5.1))
  dune
  (ppx_deriving
   (= 6.2.0)))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
