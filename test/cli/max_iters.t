  $ echo 'let x = 1' > a.ml
  $ ocamlformat --max-iters=1 a.ml --enable-outside-detected-project
  let x = 1

  $ echo 'let x     = 1' > a.ml
  $ ocamlformat --max-iters=1 a.ml --enable-outside-detected-project
  ocamlformat: "a.ml" was not already formatted. ([max-iters = 1])
  [1]
