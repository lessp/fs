(lang dune 3.10)
(name fs)
(generate_opam_files true)
(source
 (github lessp/fs))
(authors "lessp")
(maintainers "lessp")
(license MIT)

(documentation https://github.com/lessp/fs)

(package
 (name fs)
 (synopsis "A simple file system module")
 (description "Easy to use file system module")
 (depends ocaml dune bos fpath alcotest)
 (tags
  (topics fs ocaml file-system)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
