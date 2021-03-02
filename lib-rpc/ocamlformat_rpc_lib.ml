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

module Csexp = Csexp.Make (Sexp)

module Init : V with type t = [`Halt | `Unknown | `Version of string] =
struct
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
          | `Config of (string * string) list
          | `Format of string ] = struct
  type t =
    [ `Halt
    | `Unknown
    | `Error of string
    | `Config of (string * string) list
    | `Format of string ]

  let read_input in_channel =
    let open Sexp in
    match Csexp.input in_channel with
    | Ok (List [Atom "Format"; Atom x]) -> `Format x
    | Ok (List [Atom "Config"; List l]) ->
        let c =
          List.fold_left
            (fun acc -> function
              | List [Atom name; Atom value] -> (name, value) :: acc
              | _ -> acc )
            [] l
          |> List.rev
        in
        `Config c
    | Ok (List [Atom "Error"; Atom x]) -> `Error x
    | Ok (Atom "Halt") -> `Halt
    | Ok _ -> `Unknown
    | Error _msg -> `Halt

  let to_sexp =
    let open Sexp in
    function
    | `Format x -> List [Atom "Format"; Atom x]
    | `Config c ->
        let l =
          List.map (fun (name, value) -> List [Atom name; Atom value]) c
        in
        List [Atom "Config"; List l]
    | `Error x -> List [Atom "Error"; Atom x]
    | `Halt -> Atom "Halt"
    | _ -> assert false

  let output channel t =
    to_sexp t |> Csexp.to_channel channel ;
    Stdlib.flush channel
end

let get_impl = function "v1" | "V1" -> Some (module V1 : V) | _ -> None

let get_impl_exn x =
  match get_impl x with Some x -> Ok x | None -> failwith "impossible"

let pick_impl input output versions =
  let err = Error (`Msg "Version negociation failed") in
  let rec aux = function
    | [] -> err
    | latest :: others -> (
        let version = `Version latest in
        Csexp.to_channel output (Init.to_sexp version) ;
        flush output ;
        match Init.read_input input with
        | `Version v when v = latest -> get_impl_exn v
        | `Version v -> (
          match others with
          | h :: _ when v = h -> get_impl_exn v
          | _ -> aux others )
        | `Unknown -> aux others
        | `Halt -> err )
  in
  aux versions
