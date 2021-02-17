(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

let map_flatten l ~f =
  List.rev
  @@ List.fold_left ~init:[] l ~f:(fun acc str ->
         List.rev_append (f str) acc )

let rec first_non_empty = function
  | [] -> None
  | "" :: t -> first_non_empty t
  | h :: _ -> Some h

module Line = struct
  let parse str =
    let lexbuf = Lexing.from_string str in
    let rec loop acc =
      match Lexer.token_with_comments lexbuf with
      | (exception Lexer.Error _) | Parser.EOF -> Parser.EOF :: acc
      | tok -> loop (tok :: acc)
    in
    List.rev @@ loop []

  let starts_new_item str =
    match parse str with
    | [] -> false
    | (LET | MODULE | CLASS | TYPE | EXCEPTION) :: _ -> true
    | _ -> false

  let expects_followup str =
    match List.rev (parse str) with
    | [] -> false
    | (IN | LPAREN | LBRACKET | STRUCT | SIG | BEGIN) :: _ -> true
    | _ -> false

  let starts_with tok str =
    match parse str with t :: _ when Poly.(t = tok) -> true | _ -> false

  let indent x = String.(length x - length (lstrip x))

  (* only oneliners for now *)
  let is_cmt x =
    String.is_prefix x ~prefix:"(*" && String.is_suffix x ~suffix:"*)"
end

let cuts (x : string Location.loc) : string Location.loc list =
  let rec aux acc (loc_start : Lexing.position) = function
    | [] -> acc
    | h :: t ->
        let loc_end : Lexing.position =
          {loc_start with pos_cnum= loc_start.pos_cnum + String.length h}
        in
        let loc : Location.t = {loc_ghost= false; loc_start; loc_end} in
        let acc = Location.{txt= h; loc} :: acc in
        let pos =
          { loc_end with
            pos_lnum= loc_end.pos_lnum + 1
          ; pos_bol= loc_end.pos_cnum + 1
          ; pos_cnum= loc_end.pos_cnum + 1 }
        in
        aux acc pos t
  in
  aux [] x.loc.loc_start
    (Astring.String.cuts ~rev:false ~empty:true ~sep:"\n" x.txt)
  |> List.rev

let concat (str : string Location.loc) = function
  | [] -> [str]
  | (x : string Location.loc) :: xs ->
      {txt= x.txt ^ str.txt; loc= {x.loc with loc_end= str.loc.loc_end}}
      :: xs

let add_all ~from ~to_:init =
  List.fold_left from ~init ~f:(fun acc x -> x :: acc)

let split_on_linebreaks lexed : string Location.loc list =
  let open Location in
  let loc_ghost = false in
  let pos_fname = "_none_" in
  let rec aux acc ~break = function
    | [] ->
        if break then
          match acc with
          | {loc; _} :: _ ->
              let pos =
                { loc.loc_end with
                  pos_lnum= loc.loc_end.pos_lnum + 1
                ; pos_bol= loc.loc_end.pos_cnum + 1
                ; pos_cnum= loc.loc_end.pos_cnum + 1 }
              in
              {txt= ""; loc= {loc with loc_start= pos; loc_end= pos}} :: acc
          | [] ->
              let pos : Lexing.position =
                {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
              in
              [{txt= ""; loc= {loc_ghost; loc_start= pos; loc_end= pos}}]
        else acc
    | Cmt_lexer.Cmt x :: xs ->
        aux (if break then x :: acc else concat x acc) ~break:false xs
    | Cmt_lexer.S x :: xs -> (
      match cuts x with
      | [] -> impossible "should at least contain an empty string"
      | x :: others as cuts -> (
        match List.last_exn cuts with
        | {txt= ""; _} -> (
          match List.rev (List.tl_exn (List.rev cuts)) with
          | [] -> aux acc ~break:true xs
          | x :: others ->
              let acc = add_all ~from:others ~to_:(concat x acc) in
              aux acc ~break:true xs )
        | _ ->
            let acc = add_all ~from:others ~to_:(concat x acc) in
            aux acc ~break:false xs ) )
  in
  List.rev (aux [] ~break:false lexed)

let split input ~f =
  Cmt_lexer.lex_comments input
  |> split_on_linebreaks |> f |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.rev

let concat prev_lines = List.rev prev_lines |> String.concat ~sep:"\n"

let split_according_to_tokens input ~range:_ =
  let rec aux ~ret ~prev_lines = function
    | [] -> concat prev_lines :: ret
    | Location.{txt= line; _} :: t -> (
      match first_non_empty prev_lines with
      | None -> aux ~ret ~prev_lines:[line] t
      | Some last -> (
        match first_non_empty (List.rev prev_lines) with
        | None -> impossible "filtered by previous pattern matching"
        | Some first -> (
          match line with
          | "" -> (
            match t with
            | {txt= cmt; _} :: {txt= line; _} :: t
              when Line.is_cmt cmt && Line.starts_new_item line
                   && Line.indent line = Line.indent first
                   && Line.indent line <= Line.indent last
                   && not (Line.expects_followup last) ->
                aux ~ret:(concat prev_lines :: ret) ~prev_lines:[line; cmt] t
            | _ -> aux ~ret ~prev_lines:(line :: prev_lines) t )
          | _ ->
              if
                Line.starts_new_item line
                && Line.indent line = Line.indent first
                && Line.indent line <= Line.indent last
                && not (Line.expects_followup last)
              then aux ~ret:(concat prev_lines :: ret) ~prev_lines:[line] t
              else aux ~ret ~prev_lines:(line :: prev_lines) t ) ) )
  in
  split input ~f:(aux ~ret:[] ~prev_lines:[])

let split_according_to_semisemi input ~range:_ =
  let rec aux ~ret ~prev_lines = function
    | [] -> concat prev_lines :: ret
    | Location.{txt= line; _} :: t ->
        if Line.starts_with Parser.SEMISEMI line && Line.indent line = 0 then
          aux ~ret:(concat prev_lines :: ret) ~prev_lines:[line] t
        else aux ~ret ~prev_lines:(line :: prev_lines) t
  in
  split input ~f:(aux ~ret:[] ~prev_lines:[])

let split_toplevel input ~range =
  split_according_to_semisemi input ~range
  |> map_flatten ~f:(split_according_to_tokens ~range)

let merge =
  List.fold_left ~init:"" ~f:(fun x y ->
      match x with "" -> y | _ -> x ^ "\n\n" ^ y )

let fragment (type a) (fg : a Migrate_ast.Traverse.fragment) ~range input =
  ( match fg with
  | Structure -> split_toplevel input ~range
  | Use_file -> split_toplevel input ~range
  | Signature -> split_according_to_tokens input ~range )
  |> merge
