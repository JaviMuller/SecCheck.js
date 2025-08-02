(lang dune 3.17)

; (map_workspace_root false) ; used for debugging with earlybird

(using menhir 3.0)

(name sec-check-js)

(generate_opam_files true)

(source
 (github JaviMuller/SecCheck.js))

(authors "André Nascimento <email@email.com>"
         "Javier de Muller <javier.muller.santa.maria@gmail.com")

(maintainers "André Nascimento <email@email.com>"
             "Javier de Muller <javier.muller.santa.maria@gmail.com")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name sec-check-js)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml (>= "5.0.0"))
   dune
   menhir
   menhirLib
   (prelude (>= "0.5"))
   ppx_import
   ppx_deriving)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
