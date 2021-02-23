open Ocamlformat_lib.Slicer
open Ocamlformat_lib.Migrate_ast.Location

let test_split =
  let make_test name ~range ~input ~split_on_semisemi ~expected ~new_range =
    let test_name = "split: " ^ name in
    let test_fun () =
      let actual = split input ~range ~split_on_semisemi in
      Alcotest.(check (pair string (pair int int)))
        test_name (expected, new_range) actual
    in
    (test_name, `Quick, test_fun)
  in
  let loc_ghost = false in
  let pos_fname = "_none_" in
  [ make_test "empty" ~range:(1, 1) ~input:[] ~expected:"" ~new_range:(1, 1)
      ~split_on_semisemi:true
  ; make_test "multi empty" ~range:(1, 3) ~split_on_semisemi:true
      ~input:
        [ S
            { txt= "\n\n"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 2
                    ; pos_bol= 2
                    ; pos_cnum= 2 } } } ]
      ~expected:"\n\n" ~new_range:(1, 3)
  ; make_test "cmt before" ~range:(1, 3) ~split_on_semisemi:true
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
      ~expected:"(* fooooooooooooo *)\nbar\n" ~new_range:(1, 3)
  ; make_test "cmt after" ~range:(1, 2) ~split_on_semisemi:true
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
      ~expected:"foo\n(* bar *)" ~new_range:(1, 2)
  ; make_test "2 cmts" ~range:(1, 1) ~split_on_semisemi:true
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
      ~expected:"(* foo *)(* bar *)" ~new_range:(1, 1)
  ; make_test "2 cmts break" ~range:(1, 2) ~split_on_semisemi:true
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
      ~expected:"(* foo *)\n(* bar *)" ~new_range:(1, 2)
  ; make_test "let after module" ~range:(8, 8) ~split_on_semisemi:true
      ~input:
        [ S
            { txt=
                {|
module X = struct
  let x = [

  let y = bar
end

let f =
|}
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 8
                    ; pos_bol= 59
                    ; pos_cnum= 59 } } } ]
      ~expected:"let f =\n" ~new_range:(1, 1)
  ; make_test "2 let 1l split" ~range:(1, 1) ~split_on_semisemi:true
      ~input:
        [ S
            { txt= "let x = x\nlet y = y"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 1
                    ; pos_bol= 10
                    ; pos_cnum= 19 } } } ]
      ~expected:"let x = x\nlet y = y" ~new_range:(1, 1)
  ; make_test "already formatted" ~range:(1, 7) ~split_on_semisemi:true
      ~input:
        [ S
            { txt=
                {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar
|}
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 7
                    ; pos_bol= 112
                    ; pos_cnum= 112 } } } ]
      ~expected:
        {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar
|}
      ~new_range:(1, 7) ]

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
  ; make_test "multi empty" ~range:(1, 3) ~input:"\n\n" ~expected:"\n\n"
      ~new_range:(1, 3)
  ; make_test "invalid let" ~range:(1, 1) ~input:"let x" ~expected:"let x"
      ~new_range:(1, 1)
  ; make_test "valid let" ~range:(1, 1) ~input:"let x = 2"
      ~expected:"let x = 2" ~new_range:(1, 1)
  ; make_test "2 let 1l split (a)" ~range:(1, 2)
      ~input:"let x = x\nlet y = y" ~expected:"let x = x\nlet y = y"
      ~new_range:(1, 2)
  ; make_test "2 let 1l split (b)" ~range:(1, 1)
      ~input:"let x = x\nlet y = y" ~expected:"let x = x\nlet y = y"
      ~new_range:(1, 1)
  ; make_test "2 let 1l split (c)" ~range:(2, 2)
      ~input:"let x = x\nlet y = y" ~expected:"let x = x\nlet y = y"
      ~new_range:(2, 2)
  ; make_test "2 let 2l split (a)" ~range:(1, 3)
      ~input:"let x = x\n\nlet y = y" ~expected:"let x = x\n\nlet y = y"
      ~new_range:(1, 3)
  ; make_test "2 let 2l split (b)" ~range:(1, 2)
      ~input:"let x = x\n\nlet y = y" ~expected:"let x = x\n\nlet y = y"
      ~new_range:(1, 2)
  ; make_test "2 let 2l split (c)" ~range:(2, 3)
      ~input:"let x = x\n\nlet y = y" ~expected:"let x = x\n\nlet y = y"
      ~new_range:(2, 3)
  ; make_test "2 let 2l split (d)" ~range:(3, 3)
      ~input:"let x = x\n\nlet y = y" ~expected:"let y = y" ~new_range:(1, 1)
  ; make_test "2 let mix split" ~range:(1, 4)
      ~input:"let x =\n\nx\nlet y = y" ~expected:"let x =\n\nx\nlet y = y"
      ~new_range:(1, 4)
  ; make_test "let after module" ~range:(8, 8)
      ~input:{|
module X = struct
  let x = [

  let y = bar
end

let f =
|}
      ~expected:"let f =\n" ~new_range:(1, 1)
  ; make_test "already formatted" ~range:(1, 7)
      ~input:
        {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar
|}
      ~expected:
        {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar
|}
      ~new_range:(1, 7) ]

let test_interface =
  let make_test name ~range ~input ~expected ~new_range =
    let test_name = "interface: " ^ name in
    let test_fun () =
      let actual = fragment Signature input ~range in
      Alcotest.(check (pair string (pair int int)))
        test_name (expected, new_range) actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~range:(1, 1) ~input:"" ~expected:"" ~new_range:(1, 1)
  ; make_test "multi empty" ~range:(1, 3) ~input:"\n\n" ~expected:"\n\n"
      ~new_range:(1, 3) ]

let tests = test_split @ test_use_file @ test_interface
