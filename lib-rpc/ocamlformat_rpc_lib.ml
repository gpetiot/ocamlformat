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

module V1 = struct
  module Csexp = Csexp.Make (Sexp)

  module Commands = struct
    type t = Format_type of string | Halt | Unknown

    let read_input in_channel =
      let open Sexp in
      match Csexp.input in_channel with
      | Ok (List [Atom "Format_type"; Atom typ]) -> Format_type typ
      | Ok (Atom "Halt") -> Halt
      | Ok _ -> Unknown
      | Error _msg -> Halt

    let to_sexp =
      let open Sexp in
      function
      | Format_type typ -> List [Atom "Format_type"; Atom typ]
      | _ -> assert false
  end
end
