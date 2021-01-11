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
ignore Fmt_ast.fmt_fragment

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

let rec rpc_main () =
  match Rpc.V1.Commands.read_input stdin with
  | Halt -> Ok ()
  | Format_type ty -> ignore ty ; rpc_main ()
  | Unknown -> rpc_main ()

open Cmdliner

let info =
  let doc = "RPC mode for OCamlFormat, a tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P
        "$(tname) listens to RPC requests, provided on the standard input, \
         and prints the response on the standard output."
    ; `S Cmdliner.Manpage.s_commands
    ; `P "The supported RPC commands are:"
    ; `P "$(b,halt)"
    ; `P "$(b,halt) closes the connection to the RPC."
    ; `P "$(b,format-type) $(i,CSEXP)"
    ; `P
        "$(b,format-type) submits a canonical s-expression $(i,CSEXP) to be \
         formatted by OCamlFormat." ]
  in
  Term.info "ocamlformat-rpc" ~version:Version.version ~doc ~man

let rpc_main_t = Term.(const rpc_main $ const ())

let () = Term.exit @@ Term.eval (rpc_main_t, info)
