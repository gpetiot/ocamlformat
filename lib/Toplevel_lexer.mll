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

{
open Caml

let newline lexbuf = Lexing.new_line lexbuf
}

let eol = '\n' | eof
let ws = ' ' | '\t'

rule token = parse
 | eof           { [] }
 | '\n'          { newline lexbuf; `Output "" :: token lexbuf }
 | "# "          { let loc = Location.curr lexbuf in
                   let c = phrase [] (Buffer.create 8) lexbuf in
                   `Command (c, loc) :: token lexbuf }
 | ([^'#' '\n'] [^'\n']* as str) eol
                 { newline lexbuf; `Output str :: token lexbuf }
 | _ as c        { failwith (Printf.sprintf "unexpected character '%c'. Did you forget a space after the '#' at the start of the line?" c) }

and phrase acc buf = parse
  | ("\n"* as nl) "\n" ("  " | "\t")
      { newline lexbuf;
        for _ = 1 to (String.length nl) do
          newline lexbuf
        done;
        let nl = List.init (String.length nl) (fun _ -> "") in
        phrase (nl @ Buffer.contents buf :: acc) (Buffer.create 8) lexbuf }
  | eol      { newline lexbuf; List.rev (Buffer.contents buf :: acc) }
  | ";;" eol { newline lexbuf; List.rev ((Buffer.contents buf ^ ";;") :: acc) }
  | _ as c   { Buffer.add_char buf c; phrase acc buf lexbuf }

{
let repl_file lx =
  let x =
    try
      newline lx;
      token lx
    with _ -> raise (Syntaxerr.Error (Expecting (Location.curr lx, ";;")))
  in
  let open Ocaml_413_extended.Parsetree in
  List.fold_left (fun acc -> function
      | `Command (lines, _loc) ->
          { prepl_phrase=
              String.concat "\n" lines
              |> Lexing.from_string |> Parse.toplevel_phrase
          ; prepl_output= "" }
          :: acc
      | `Output line -> (
          match acc with
          | [] -> raise (Syntaxerr.Error (Expecting (Location.curr lx, "#")))
          | {prepl_phrase; prepl_output} :: t ->
              {prepl_phrase; prepl_output= prepl_output ^ line}
              :: t )
    ) [] x
  |> List.rev
}
