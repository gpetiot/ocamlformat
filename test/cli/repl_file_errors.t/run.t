Make sure the locations of errors in repl files are right.

  $ ocamlformat --repl-file line5.repl
  ocamlformat: ignoring "line5.repl" (syntax error)
  File "line5.repl", line 5, characters 5-5:
  Error: Syntax error
  [1]

  $ ocamlformat --repl-file line6.repl
  ocamlformat: ignoring "line6.repl" (syntax error)
  File "line6.repl", line 6, characters 5-5:
  Error: Syntax error
  [1]

  $ ocamlformat --repl-file line7.repl
  ocamlformat: ignoring "line7.repl" (syntax error)
  File "line7.repl", line 7, characters 5-5:
  Error: Syntax error
  [1]

  $ ocamlformat --repl-file missing_semisemi.repl
  ocamlformat: ignoring "missing_semisemi.repl" (syntax error)
  File "missing_semisemi.repl", line 2, characters 3-3:
  Error: Syntax error
  [1]

  $ ocamlformat --repl-file empty_line_begin.repl
  ocamlformat: ignoring "empty_line_begin.repl" (syntax error)
  File "empty_line_begin.repl", line 4, characters 0-0:
  Error: Syntax error: # expected.
  [1]
