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

module type V = sig
  type t

  val read_input : Stdlib.in_channel -> t

  val to_sexp : t -> Sexplib0.Sexp.t

  val output : Stdlib.out_channel -> t -> unit
end

(** Version used to set the protocol version *)
module Init : V with type t = [`Halt | `Unknown | `Version of string]

module V1 :
  V
    with type t =
          [ `Halt
          | `Unknown
          | `Format_signature of string
          | `Format_toplevel_phrase of string ]
