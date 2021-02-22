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
      ) ]

let tests = test_numeric
