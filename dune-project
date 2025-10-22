(lang dune 3.20)

(name sqc)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Thomas Zachary <thomaszachary06@proton.me>")

(license LICENSE)

(package
 (name sqc)
 (synopsis "A funny little compiler")
 (description "How funny")
 (depends ocaml base ppx_jane base_trie ppx_deriving))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
