open Ocamlformat_lib.Cmt_lexer
open Ocamlformat_lib.Migrate_ast.Location

let lex =
  let pp fs = function
    | Cmt x -> Fmt.pf fs "(Cmt %a)" (pp_loc Fmt.string) x
    | S x -> Fmt.pf fs "(S %a)" (pp_loc Fmt.string) x
  in
  Alcotest.testable pp ( = )

let test_lex_comments =
  let make_test name ~input ~expected =
    let test_name = "lex_comments: " ^ name in
    let test_fun () =
      let actual = lex_comments input in
      Alcotest.(check (list lex)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  let loc_ghost = false in
  let pos_fname = "_none_" in
  [ make_test "empty" ~input:"" ~expected:[]
  ; make_test "cmt before" ~input:"(* fooooooooooooo *)\nbar\n"
      ~expected:
        [ Cmt
            { txt= "(* fooooooooooooo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 20}
                } }
        ; S
            { txt= "\nbar\n"
            ; loc=
                { loc_ghost
                ; loc_start=
                    {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 20}
                ; loc_end= {pos_fname; pos_lnum= 2; pos_bol= 25; pos_cnum= 25}
                } } ]
  ; make_test "cmt multi before" ~input:"(* foooooo\n fooooooooooooo *)\nbar"
      ~expected:
        [ Cmt
            { txt= "(* foooooo\n fooooooooooooo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 11; pos_cnum= 29}
                } }
        ; S
            { txt= "\nbar"
            ; loc=
                { loc_ghost
                ; loc_start=
                    {pos_fname; pos_lnum= 1; pos_bol= 11; pos_cnum= 29}
                ; loc_end= {pos_fname; pos_lnum= 2; pos_bol= 30; pos_cnum= 33}
                } } ]
  ; make_test "cmt after" ~input:"foo\n(* bar *)"
      ~expected:
        [ S
            { txt= "foo\n"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 4}
                } }
        ; Cmt
            { txt= "(* bar *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 4}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 13}
                } } ]
  ; make_test "cmt multi after" ~input:"fooooo\n(* baaaaaar\n  baaaaaar *)"
      ~expected:
        [ S
            { txt= "fooooo\n"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 7; pos_cnum= 7}
                } }
        ; Cmt
            { txt= "(* baaaaaar\n  baaaaaar *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 1; pos_bol= 7; pos_cnum= 7}
                ; loc_end= {pos_fname; pos_lnum= 2; pos_bol= 19; pos_cnum= 32}
                } } ]
  ; make_test "2 cmts" ~input:"(* foo *)(* bar *)"
      ~expected:
        [ Cmt
            { txt= "(* foo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                } }
        ; Cmt
            { txt= "(* bar *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 18}
                } } ]
  ; make_test "2 cmts break" ~input:"(* foo *)\n(* bar *)"
      ~expected:
        [ Cmt
            { txt= "(* foo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                } }
        ; S
            { txt= "\n"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 10}
                } }
        ; Cmt
            { txt= "(* bar *)"
            ; loc=
                { loc_ghost
                ; loc_start=
                    {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 10}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 19}
                } } ] ]

let tests = test_lex_comments
