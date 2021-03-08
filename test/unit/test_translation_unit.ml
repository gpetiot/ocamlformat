open Base

let reindent ~source ~range:(low, high) indents =
  let lines = String.split_lines source in
  let low = low - 1 and high = high - 1 in
  let lines =
    List.mapi lines ~f:(fun i line ->
        if i < low then line
        else if low <= i && i <= high then
          let indent = List.nth_exn indents (i - low) in
          let line = String.lstrip line in
          let spaces = String.make indent ' ' in
          spaces ^ line
        else line )
  in
  String.concat ~sep:"\n" lines

let errmsg =
  let pp fs (`Msg s) = Fmt.pf fs "%s" s in
  Alcotest.testable pp Poly.equal

let test_numeric =
  let make_test name ~source ~range expected =
    let test_name = "numeric: " ^ name in
    ( test_name
    , `Quick
    , fun () ->
        let open Ocamlformat_lib in
        let opts = Conf.{debug= false; margin_check= false} in
        let got =
          Translation_unit.numeric Use_file ~input_name:"_" ~source ~range
            Conf.default_profile opts
          |> Result.map ~f:(reindent ~source ~range)
        in
        Alcotest.check Alcotest.(result string errmsg) test_name expected got
    )
  in
  [ make_test "invalid low" ~source:"foo\nbar" ~range:(0, 1)
      (Error (`Msg "Invalid line number 0."))
  ; make_test "invalid high" ~source:"foo\nbar" ~range:(1, 4)
      (Error (`Msg "Invalid line number 4."))
  ; make_test "invalid range" ~source:"foo\nbar" ~range:(2, 1)
      (Error (`Msg "Invalid range 2-1."))
  ; make_test "empty buffer" ~source:"" ~range:(1, 1) (Ok "")
  ; make_test "last buffer line" ~source:"foo\nbar" ~range:(2, 3)
      (Ok "foo\nbar")
  ; make_test "already formatted"
      ~source:
        {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar
|}
      ~range:(1, 7)
      (Ok
         {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar|}
      )
  ; make_test "not already formatted"
      ~source:
        {|let foooooooooooo = let foooooooo = foooooooooooo in foooooooooooo

let foooooooooooo = let foooooooooooooo =
let woooooooooooooooo = koooooooooooooooo in
baaaaaar
in
hooohoooo
|}
      ~range:(1, 7)
      (Ok
         {|let foooooooooooo = let foooooooo = foooooooooooo in foooooooooooo

let foooooooooooo = let foooooooooooooo =
    let woooooooooooooooo = koooooooooooooooo in
    baaaaaar
  in
  hooohoooo|}
      )
  ; make_test "with parens and begin/end"
      ~source:
        {|let x = begin
    let y =
      (if (k = x)
       then
         begin match k,v with [x; y] -> ( foo;
                                        (if (z) then foo else bar) )
         end
       else
         foo)
    in
    foooooo
  end|}
      ~range:(1, 12)
      (Ok
         {|let x = begin
  let y =
    (if (k = x)
    then
      begin match k,v with [x; y] -> ( foo;
          (if (z) then foo else bar) )
    end
    else
      foo)
  in
  foooooo
  end|}
      )
  ; make_test "split over multiple lines"
      ~source:{|let fooooo =
[
foooooo ;
foooooooo ;
fooooooo
]|}
      ~range:(1, 6)
      (Ok {|let fooooo =
  [
    foooooo ;
    foooooooo ;
    fooooooo
]|})
  ; make_test "invalid file"
      ~source:{|let foooooo =
let foooooooooooo =
(
[
fun x ->
foooooo|}
      ~range:(1, 6)
      (Ok
         {|let foooooo =
  let foooooooooooo =
    (
      [
        fun x ->
          foooooo|}
      )
  ; make_test "already formatted function"
      ~source:
        {|let fmt_expressions c width sub_exp exprs fmt_expr
    (p : Params.elements_collection) =
  match c.conf.break_collection_expressions with
  | `Fit_or_vertical -> fmt_elements_collection p fmt_expr exprs
  | `Wrap ->
      let is_simple x = is_simple c.conf width (sub_exp x) in
      let break x1 x2 = not (is_simple x1 && is_simple x2) in
      let grps = List.group exprs ~break in
      let fmt_grp ~first:first_grp ~last:last_grp exprs =
        fmt_elements_collection ~first_sep:first_grp ~last_sep:last_grp p
          fmt_expr exprs
      in
      list_fl grps fmt_grp|}
      ~range:(7, 7)
      (Ok
         {|let fmt_expressions c width sub_exp exprs fmt_expr
    (p : Params.elements_collection) =
  match c.conf.break_collection_expressions with
  | `Fit_or_vertical -> fmt_elements_collection p fmt_expr exprs
  | `Wrap ->
      let is_simple x = is_simple c.conf width (sub_exp x) in
      let break x1 x2 = not (is_simple x1 && is_simple x2) in
      let grps = List.group exprs ~break in
      let fmt_grp ~first:first_grp ~last:last_grp exprs =
        fmt_elements_collection ~first_sep:first_grp ~last_sep:last_grp p
          fmt_expr exprs
      in
      list_fl grps fmt_grp|}
      ) ]

let test_numeric_file =
  let fmt_ast_source = Stdio.In_channel.read_all "../../lib/Fmt_ast.ml" in
  let make_test name ~source ~range ~expected =
    let test_name = "numeric (file): " ^ name in
    ( test_name
    , `Quick
    , fun () ->
        let open Ocamlformat_lib in
        let opts = Conf.{debug= false; margin_check= false} in
        let got =
          Translation_unit.numeric Use_file ~input_name:"_" ~source ~range
            Conf.default_profile opts
        in
        Alcotest.check
          Alcotest.(result (list int) errmsg)
          test_name expected got )
  in
  [ make_test "fmt_ast.ml (26)" ~source:fmt_ast_source ~range:(26, 26)
      ~expected:(Ok [2])
  ; make_test "fmt_ast.ml (111)" ~source:fmt_ast_source ~range:(111, 111)
      ~expected:(Ok [6]) ]

let tests = test_numeric @ test_numeric_file
