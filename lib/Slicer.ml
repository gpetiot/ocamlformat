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
  | Location.{txt= ""; _} :: t -> first_non_empty t
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

  let break_according_to_tokens ~first ~last line =
    starts_new_item line
    && indent line = indent first.Location.txt
    && indent line <= indent last.Location.txt
    && not (expects_followup last.txt)

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

let concat x dst =
  match x with
  | [] -> dst
  | [s] -> s :: dst
  | (h : string Location.loc) :: t ->
      List.fold_left t ~init:h ~f:(fun acc (x : string Location.loc) ->
          { txt= x.txt ^ "\n" ^ acc.txt
          ; loc= {acc.loc with loc_start= x.loc.loc_start} } )
      :: dst

let split input ~f ~range =
  let rec aux ~ret ~prev_lines = function
    | [] -> concat prev_lines ret
    | h :: t ->
        let prev_lines, ret, t = f ~prev_lines ~ret ~range h t in
        aux ~ret ~prev_lines t
  in
  Cmt_lexer.lex_comments input
  |> split_on_linebreaks
  |> aux ~ret:[] ~prev_lines:[]
  |> List.map ~f:(fun Location.{txt; _} -> String.strip txt)
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.rev

let split_according_to_tokens ~prev_lines ~ret ~range:(low, high)
    (line : string Location.loc) (t : string Location.loc list) =
  match first_non_empty prev_lines with
  | None -> ([line], ret, t)
  | Some last -> (
    match first_non_empty (List.rev prev_lines) with
    | None -> impossible "filtered by previous pattern matching"
    | Some first -> (
      match line.txt with
      | "" -> (
        match t with
        | cmt :: line :: t
          when Line.is_cmt cmt.txt
               && Line.break_according_to_tokens ~first ~last line.txt ->
            let before = concat prev_lines ret in
            let range_starts_after =
              match before with
              | [] -> true
              | x :: _ -> x.loc.loc_end.pos_lnum + 1 < low
            in
            if range_starts_after then
              (* The [range] starts after this item, we can discard the items
                 already added in [ret]. *)
              ([line; cmt], [], t)
            else if cmt.loc.loc_start.pos_lnum + 1 > high then
              (* The [range] ends before this item, we can discard what comes
                 after. *)
              ([], before, [])
            else ([line; cmt], before, t)
        | _ -> (line :: prev_lines, ret, t) )
      | _ ->
          if Line.break_according_to_tokens ~first ~last line.txt then
            let before = concat prev_lines ret in
            let range_starts_after =
              match before with
              | [] -> true
              | x :: _ -> x.loc.loc_end.pos_lnum + 1 < low
            in
            if range_starts_after then
              (* The [range] starts after this item, we can discard the items
                 already added in [ret]. *)
              ([line], [], t)
            else if line.loc.loc_start.pos_lnum + 1 > high then
              (* The [range] ends before this item, we can discard what comes
                 after. *)
              ([], concat prev_lines ret, [])
            else ([line], concat prev_lines ret, t)
          else (line :: prev_lines, ret, t) ) )

let split_according_to_semisemi ~prev_lines ~ret ~range:(low, high)
    (line : string Location.loc) t =
  if Line.starts_with Parser.SEMISEMI line.txt && Line.indent line.txt = 0
  then
    let before = concat prev_lines ret in
    let range_starts_after =
      match before with
      | [] -> true
      | x :: _ -> x.loc.loc_end.pos_lnum + 1 < low
    in
    if range_starts_after then
      (* The [range] starts after this item, we can discard the items already
         added in [ret]. *)
      ([line], [], t)
    else if line.loc.loc_start.pos_lnum + 1 > high then
      (* The [range] ends before this item, we can discard what comes after. *)
      ([], concat prev_lines ret, [])
    else ([line], concat prev_lines ret, t)
  else (line :: prev_lines, ret, t)

let split_nontoplevel = split ~f:split_according_to_tokens

let split_toplevel input ~range =
  split ~f:split_according_to_semisemi input ~range
  |> map_flatten ~f:(split_nontoplevel ~range)

let merge =
  List.fold_left ~init:"" ~f:(fun x y ->
      match x with "" -> y | _ -> x ^ "\n\n" ^ y )

let fragment (type a) (fg : a Migrate_ast.Traverse.fragment) ~range input =
  ( match fg with
  | Structure | Use_file -> split_toplevel input ~range
  | Signature -> split_nontoplevel input ~range )
  |> merge
