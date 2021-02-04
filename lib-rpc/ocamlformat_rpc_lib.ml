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

open Sexplib0

module type V = sig
  type t

  val read_input : Stdlib.in_channel -> t

  val to_sexp : t -> Sexp.t

  val output : Stdlib.out_channel -> t -> unit
end

module Init : V with type t = [`Halt | `Unknown | `Version of string] =
struct
  module Csexp = Csexp.Make (Sexp)

  type t = [`Halt | `Unknown | `Version of string]

  let read_input in_channel =
    let open Sexp in
    match Csexp.input in_channel with
    | Ok (Atom "Halt") -> `Halt
    | Ok (List [Atom "Version"; Atom v]) -> `Version v
    | Ok _ -> `Unknown
    | Error _msg -> `Halt

  let to_sexp =
    let open Sexp in
    function
    | `Version v -> List [Atom "Version"; Atom v] | _ -> assert false

  let output channel t =
    to_sexp t |> Csexp.to_channel channel ;
    Stdlib.flush channel
end

module V1 :
  V
    with type t =
          [ `Halt
          | `Unknown
          | `Error of string
          | `Format_signature of string
          | `Format_toplevel_phrase of string ] = struct
  module Csexp = Csexp.Make (Sexp)

  type t =
    [ `Halt
    | `Unknown
    | `Error of string
    | `Format_signature of string
    | `Format_toplevel_phrase of string ]

  let read_input in_channel =
    let open Sexp in
    match Csexp.input in_channel with
    | Ok (List [Atom "Format_signature"; Atom x]) -> `Format_signature x
    | Ok (List [Atom "Format_toplevel_phrase"; Atom x]) ->
        `Format_toplevel_phrase x
    | Ok (List [Atom "Error"; Atom x])-> `Error x
    | Ok (Atom "Halt") -> `Halt
    | Ok _ -> `Unknown
    | Error _msg -> `Halt

  let to_sexp =
    let open Sexp in
    function
    | `Format_signature x -> List [Atom "Format_signature"; Atom x]
    | `Format_toplevel_phrase x ->
        List [Atom "Format_toplevel_phrase"; Atom x]
    | `Error x -> List [Atom "Error"; Atom x]
    | _ -> assert false

  let output channel t =
    to_sexp t |> Csexp.to_channel channel ;
    Stdlib.flush channel
end
