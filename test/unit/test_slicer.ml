open Ocamlformat_lib.Slicer
open Ocamlformat_lib.Migrate_ast.Location

let test_use_file =
  let make_test name ~range ~input ~expected ~new_range =
    let test_name = "use_file: " ^ name in
    let test_fun () =
      let actual = fragment Use_file input ~range in
      Alcotest.(check (pair string (pair int int)))
        test_name (expected, new_range) actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~range:(1, 1) ~input:"" ~expected:"" ~new_range:(1, 1)
  ; make_test "multi empty" ~range:(1, 3) ~input:"\n\n" ~expected:""
      ~new_range:(1, 3)
  ; make_test "invalid let" ~range:(1, 1) ~input:"let x" ~expected:"let x"
      ~new_range:(1, 1)
  ; make_test "valid let" ~range:(1, 1) ~input:"let x = 2"
      ~expected:"let x = 2" ~new_range:(1, 1)
  ; make_test "2 let 1l split (a)" ~range:(1, 2)
      ~input:"let x = x\nlet y = y" ~expected:"let x = x\n\nlet y = y"
      ~new_range:(1, 2)
  ; make_test "2 let 1l split (b)" ~range:(1, 1)
      ~input:"let x = x\nlet y = y" ~expected:"let x = x" ~new_range:(1, 1)
  ; make_test "2 let 1l split (c)" ~range:(2, 2)
      ~input:"let x = x\nlet y = y" ~expected:"let y = y" ~new_range:(2, 2)
  ; make_test "2 let 2l split (a)" ~range:(1, 3)
      ~input:"let x = x\n\nlet y = y" ~expected:"let x = x\n\nlet y = y"
      ~new_range:(1, 3)
  ; make_test "2 let 2l split (b)" ~range:(1, 2)
      ~input:"let x = x\n\nlet y = y" ~expected:"let x = x" ~new_range:(1, 2)
  ; make_test "2 let 2l split (c)" ~range:(2, 3)
      ~input:"let x = x\n\nlet y = y" ~expected:"let x = x\n\nlet y = y"
      ~new_range:(2, 3)
  ; make_test "2 let 2l split (d)" ~range:(3, 3)
      ~input:"let x = x\n\nlet y = y" ~expected:"let y = y" ~new_range:(3, 3)
  ; make_test "2 let mix split" ~range:(1, 4)
      ~input:"let x =\n\nx\nlet y = y" ~expected:"let x =\n\nx\n\nlet y = y"
      ~new_range:(1, 4)
  ; make_test "" ~range:(8, 8)
      ~input:{|
module X = struct
  let x = [

  let y = bar
end

let f =
|}
      ~expected:"let f =" ~new_range:(8, 8) ]

let test_interface =
  let make_test name ~range ~input ~expected =
    let test_name = "interface: " ^ name in
    let test_fun () =
      let actual = fragment Signature input ~range in
      Alcotest.(check (pair string (pair int int))) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~range:(1, 1) ~input:"" ~expected:("", (1, 1))
  ; make_test "multi empty" ~range:(1, 3) ~input:"\n\n" ~expected:("", (1, 3))
  ]

let loc x = Alcotest.testable (pp_loc (Alcotest.pp x)) ( = )

let test_split_on_linebreaks =
  let make_test name ~input ~expected =
    let test_name = "split_on_linebreaks: " ^ name in
    let test_fun () =
      let actual = split_on_linebreaks input in
      Alcotest.(check (list (loc string))) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  let loc_ghost = false in
  let pos_fname = "_none_" in
  [ make_test "empty" ~input:[] ~expected:[]
  ; make_test "cmt before"
      ~input:
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
      ~expected:
        [ { txt= "(* fooooooooooooo *)"
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
              ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 20}
              } }
        ; { txt= "bar"
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 1; pos_bol= 21; pos_cnum= 21}
              ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 21; pos_cnum= 24}
              } }
        ; { txt= ""
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 2; pos_bol= 25; pos_cnum= 25}
              ; loc_end= {pos_fname; pos_lnum= 2; pos_bol= 25; pos_cnum= 25}
              } } ]
  ; make_test "cmt after"
      ~input:
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
      ~expected:
        [ { txt= "foo"
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
              ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 3} }
          }
        ; { txt= "(* bar *)"
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 4}
              ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 13}
              } } ]
  ; make_test "2 cmts"
      ~input:
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
      ~expected:
        [ { txt= "(* foo *)(* bar *)"
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
              ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 18}
              } } ]
  ; make_test "2 cmts break"
      ~input:
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
                } } ]
      ~expected:
        [ { txt= "(* foo *)"
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
              ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9} }
          }
        ; { txt= "(* bar *)"
          ; loc=
              { loc_ghost
              ; loc_start= {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 10}
              ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 19}
              } } ] ]

let tests = test_use_file @ test_interface @ test_split_on_linebreaks
