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

val parse :
     loc:Warnings.loc
  -> string
  -> (Odoc_parser.Ast.t, Odoc_parser.Warning.t list) Result.t

val warn : Format.formatter -> Odoc_parser.Warning.t -> unit

type error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string * string
  | Added of Location.t * string
  | Removed of Location.t * string

val is_tag_only : Odoc_parser.Ast.t -> bool
(** [true] if the documentation only contains tags *)

val normalize :
     parse_docstrings:bool
  -> normalize_code:(string -> string)
  -> string
  -> string

val normalize_text : string -> string

val is_repl_block : string -> bool
(** [is_repl_block x] returns whether [x] is a list of REPL phrases and
    outputs of the form:

    {v
      # let this is = some phrase;;
      this is some output
    v} *)
