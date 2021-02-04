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

module V = struct
  type t = V1

  let is_handled = function "v1" | "V1" -> Some V1 | _ -> None

  let propose_another = function "v1" | "V1" -> None | _ -> Some V1

  let to_string = function V1 -> "v1"
end

type state = Waiting_for_version | Version_defined of V.t

let format fg source =
  let input_name = "<rpc input>" in
  let conf = Conf.default_profile in
  let opts =
    Conf.{debug= false; margin_check= false; format_invalid_files= false}
  in
  Translation_unit.parse_and_format fg ~input_name ~source conf opts

let rec rpc_main = function
  | Waiting_for_version -> (
    match Ocamlformat_rpc_lib.Init.read_input stdin with
    | `Halt -> Ok ()
    | `Unknown -> Ok ()
    | `Version vstr -> (
      match V.is_handled vstr with
      | Some v ->
          Ocamlformat_rpc_lib.Init.output stdout (`Version vstr) ;
          Out_channel.flush stdout ;
          rpc_main (Version_defined v)
      | None -> (
        match V.propose_another vstr with
        | Some v ->
            let vstr = V.to_string v in
            Ocamlformat_rpc_lib.Init.output stdout (`Version vstr) ;
            rpc_main Waiting_for_version
        | None -> Ok () ) ) )
  | Version_defined v as state -> (
    match v with
    | V1 -> (
      match Ocamlformat_rpc_lib.V1.read_input stdin with
      | `Halt -> Ok ()
      | `Unknown | `Error _ -> rpc_main state
      | `Format_type ty ->
          ( match format Signature ty with
          | Ok formatted ->
              Ocamlformat_rpc_lib.V1.output stdout (`Format_type formatted)
          | Error e ->
            Translation_unit.print_error
              ~fmt:(Format.str_formatter)
              ~exe:"ocamlformat-rpc"
              ~debug:false
              ~quiet:false
              ~input_name:ty
              e;
              Ocamlformat_rpc_lib.V1.output stdout (`Error (Format.flush_str_formatter ()))) ;
          rpc_main state
      | `Format_toplevel_phrase tp ->
          ( match format Use_file tp with
          | Ok formatted ->
              Ocamlformat_rpc_lib.V1.output stdout
                (`Format_toplevel_phrase formatted)
          | Error e ->
            Translation_unit.print_error
              ~fmt:(Format.str_formatter)
              ~exe:"ocamlformat-rpc"
              ~debug:false
              ~quiet:false
              ~input_name:tp
              e;
              Ocamlformat_rpc_lib.V1.output stdout (`Error (Format.flush_str_formatter ()))
          ) ;
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
        "On version $(b,v1), the supported RPC commands are: $(b,Halt) to \
         close the connection to the RPC; $(b,Format_type) $(i,CSEXP): \
         submits a canonical s-expression $(i,CSEXP) to be formatted as a \
         type by OCamlFormat, the formatted output is sent as a reply of \
         the same form $(b,Format_type) $(i,CSEXP); \
         $(b,Format_toplevel_phrase) $(i,CSEXP): submits a canonical \
         s-expression $(i,CSEXP) to be formatted by OCamlFormat as a \
         toplevel phrase, the formatted output is sent as a reply of the \
         same form $(b,Format_toplevel_phrase) $(i,CSEXP). Unknown commands \
         are ignored." ]
  in
  Term.info "ocamlformat-rpc" ~version:Version.version ~doc ~man

let rpc_main_t = Term.(const rpc_main $ const ())

let () = Term.exit @@ Term.eval (rpc_main_t, info)
