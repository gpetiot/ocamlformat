  $ echo 'let x = 1' > a.ml
  $ ocamlformat --check a.ml --enable-outside-detected-project

  $ echo 'let x     =1' > a.ml
  $ ocamlformat --check a.ml --enable-outside-detected-project
  [1]

  $ echo 'let x = 1' > a.ml
  $ ocamlformat --output x.ml --check a.ml --enable-outside-detected-project
  ocamlformat: Cannot specify --output with --check
  [1]

  $ echo 'let x = 1' > a.ml
  $ ocamlformat --inplace --check a.ml --enable-outside-detected-project
  ocamlformat: Cannot specify --inplace with --check
  [1]
