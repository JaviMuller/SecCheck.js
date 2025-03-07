(lang dune 3.17)

(using menhir 3.0)

(name trusted-cpg)

(generate_opam_files true)

(source
 (github JaviMuller/TrustedCPG))

(authors "André Nascimento <email@email.com>"
         "Javier de Muller <javier.muller.santa.maria@gmail.com")

(maintainers "André Nascimento <email@email.com>"
             "Javier de Muller <javier.muller.santa.maria@gmail.com")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name trusted-cpg)
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
