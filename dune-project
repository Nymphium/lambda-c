(lang dune 2.7)
(name lambda_c)

(source (github nymphium/lambda-c))

(package
  (name lambda_c)
  (depends
    (dune (>= 2))
    (ocaml (>= 4.08))

    (utop :dev)
    (ocamlformat :dev)
    (ocaml-lsp-server :dev)))
