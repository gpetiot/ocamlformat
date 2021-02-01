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

val split_on_linebreaks : Cmt_lexer.token list -> string Location.loc list
(** [split_on_linebreaks tokens] returns a list of lines based on the
    [tokens].

    Exposed for tests. *)

val fragment :
  'a Migrate_ast.Traverse.fragment -> range:int * int -> string -> string
(** [fragment fg ~range x] slices [x] into a smaller string, filtered
    according to the range of lines [range], and containing the relevant
    context so we can infer the indentation to apply to this range of lines. *)
