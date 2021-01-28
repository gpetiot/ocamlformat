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

(** OCamlFormat-RPC *)

open Ocamlformat_lib

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

type version = V1

type state = Waiting_for_version | Version_defined of version

let version_handled = function "v1" | "V1" -> Some V1 | _ -> None

let propose_another_version = function
  | "v1" | "V1" -> None
  | _ -> Some ("v1", V1)

let rec rpc_main = function
  | Waiting_for_version -> (
    match Ocamlformat_rpc_lib.Init.read_input stdin with
    | `Halt -> Ok ()
    | `Unknown -> Ok ()
    | `Version vstr -> (
      match version_handled vstr with
      | Some v ->
          let answer = Ocamlformat_rpc_lib.Init.(to_sexp (`Version vstr)) in
          Out_channel.output_string stdout (Sexp.to_string answer) ;
          Out_channel.flush stdout ;
          rpc_main (Version_defined v)
      | None -> (
        match propose_another_version vstr with
        | Some (vstr, _v) ->
            let answer =
              Ocamlformat_rpc_lib.Init.(to_sexp (`Version vstr))
            in
            Out_channel.output_string stdout (Sexp.to_string answer) ;
            Out_channel.flush stdout ;
            rpc_main Waiting_for_version
        | None -> Ok () ) ) )
  | Version_defined v as state -> (
    match v with
    | V1 -> (
      match Ocamlformat_rpc_lib.V1.read_input stdin with
      | `Halt -> Ok ()
      | `Unknown -> rpc_main state
      | `Format_type ty ->
          let input_name = "<rpc input>" in
          let conf =
            Conf.build_config
            (* we want to format the type no matter what, so even if there is
               not any .ocamlformat file *)
              ~enable_outside_detected_project:true
              ~root:(Some (Fpath.cwd ()))
              ~file:input_name ~is_stdin:true
          in
          let opts =
            Conf.
              {debug= false; margin_check= false; format_invalid_files= false}
          in
          ( match
              Translation_unit.parse_and_format Signature ~input_name
                ~source:ty conf opts
            with
          | Ok formatted ->
              let answer =
                Ocamlformat_rpc_lib.V1.(to_sexp (`Format_type formatted))
              in
              Out_channel.output_string stdout (Sexp.to_string answer) ;
              Out_channel.flush stdout
          | Error _ -> () ) ;
          rpc_main state ) )

let rpc_main () = rpc_main Waiting_for_version

open Cmdliner

let info =
  let doc = "RPC mode for OCamlFormat, a tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P
        "$(tname) listens to RPC requests, provided on the standard input, \
         and prints the response on the standard output."
    ; `S Cmdliner.Manpage.s_commands
    ; `P
        "Before the client and the server agree on a common version to use \
         the following commands are available: $(b,Halt) to close the \
         connection to the RPC; $(b,Version) $(i,v) to ask the server to \
         use version $(i,v). If the server agrees upon the version he will \
         send the reply $(b,Version) $(i,v) and the protocol version is set \
         to $(i,v), to use another version later the client has to close \
         the connexion and start a new one. If the server cannot use \
         version $(i,v) he might propose another version $(i,w) by sending \
         the reply $(b,Version) $(i,w) that the client can accept by \
         sending the same request for version $(i,w), or propose another \
         version. If the server cannot propose another version it will \
         close the connection. Unknown commands are ignored."
    ; `P
        "Once the client and the server agree on a common version, the \
         requests you can send may differ from one version to another."
    ; `P
        "On version $(i,v1), the supported RPC commands are: $(b,Halt) to \
         close the connection to the RPC; $(b,Format_type) $(i,CSEXP): \
         submits a canonical s-expression $(i,CSEXP) to be formatted by \
         OCamlFormat. Unknown commands are ignored." ]
  in
  Term.info "ocamlformat-rpc" ~version:Version.version ~doc ~man

let rpc_main_t = Term.(const rpc_main $ const ())

let () = Term.exit @@ Term.eval (rpc_main_t, info)
