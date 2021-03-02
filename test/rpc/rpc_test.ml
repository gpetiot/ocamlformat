module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e
  end
end

open Result.Infix
module Sexp = Sexplib0.Sexp
module Csexp = Csexp.Make (Sexp)
module Ocf = Ocamlformat_rpc_lib

type process =
  {pid: int; input: in_channel; output: out_channel; impl: (module Ocf.V)}

let running_process : process option ref = ref None

let log = Format.printf

let supported_versions = ["v1"]

let start () =
  let prog = Sys.argv.(1) in
  let argv = ["ocamlformat-rpc"] in
  let stdin, in_ = Unix.pipe () in
  let out_, stdout = Unix.pipe () in
  Unix.set_close_on_exec in_ ;
  let pid =
    Spawn.spawn ~prog ~argv ~stdin ~stdout
      ~stderr:(Unix.descr_of_out_channel stderr)
      ()
  in
  let process =
    { pid
    ; input= Unix.in_channel_of_descr out_
    ; output= Unix.out_channel_of_descr in_
    ; impl= (module Ocf.V1) }
  in
  log "[ocf] proposed versions: @[<hv>%a@]\n%!"
    (Format.pp_print_list
       ~pp_sep:(fun fs () -> Format.fprintf fs ",@ ")
       Format.pp_print_string )
    supported_versions ;
  Ocf.pick_impl process.input process.output supported_versions
  >>| fun impl ->
  let process = {process with impl} in
  running_process := Some process ;
  process

let get_process () =
  match !running_process with
  | None -> start ()
  | Some p ->
      (* Is the process still running ? *)
      let i, _ = Unix.waitpid [WNOHANG] p.pid in
      if i = 0 then Ok p else start ()

module Client = struct
  module type S = sig
    val format : string -> (string, [`Msg of string]) result

    val config : (string * string) list -> (string, [`Msg of string]) result

    val halt : unit -> (unit, [`Msg of string]) result
  end

  module V1 = struct
    let query t =
      get_process ()
      >>| fun p ->
      Ocf.V1.output p.output t ;
      log "Sent %s\n%!" (Ocf.V1.to_sexp t |> Sexp.to_string) ;
      Ocf.V1.read_input p.input

    let format x =
      log "Format '%s'\n%!" x ;
      match query (`Format x) with
      | Ok (`Format x) -> Ok x
      | Ok (`Error msg) -> Error (`Msg msg)
      | _ -> Error (`Msg "Unknown error")

    let config x =
      match query (`Config x) with
      | Ok (`Config _) -> Ok "configuration updated"
      | Ok (`Error msg) -> Error (`Msg msg)
      | _ -> Error (`Msg "Unknown error")

    let halt () =
      get_process ()
      >>| fun p ->
      Ocf.V1.output p.output `Halt ;
      log "Sent %s\n%!" (Ocf.V1.to_sexp `Halt |> Sexp.to_string) ;
      close_in p.input ;
      close_out p.output ;
      running_process := None
  end
end

let protect_unit x =
  match x with Ok () -> () | Error (`Msg e) -> log "Error: %s\n%!" e

let protect_string x =
  match x with
  | Ok s -> log "@[<hv>Output:@;%s@]\n%!" s
  | Error (`Msg e) -> log "Error: %s\n%!" e

let get_client () : (module Client.S) =
  match get_process () with
  | Ok p -> (
    match p.impl with
    | (module V1) ->
        log "[ocf] implementation v1 selected\n%!" ;
        (module Client.V1) )
  | Error (`Msg msg) ->
      log "[ocf] no implementation selected\n%!" ;
      failwith msg

let () =
  log "Starting then doing nothing\n%!" ;
  let module C = (val get_client ()) in
  protect_unit @@ C.halt ()

let () =
  log "Sending Type requests\n%!" ;
  let module C = (val get_client ()) in
  protect_string @@ C.format "char -> string" ;
  protect_string @@ C.format "int -> int" ;
  protect_string @@ C.format " int    (* foo *) \n\n ->     int  (* bar *)" ;
  protect_string @@ C.config [("foo", "bar")] ;
  protect_string @@ C.config [("margin", "10")] ;
  protect_string @@ C.format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ C.config [("margin", "80")] ;
  protect_string @@ C.format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ C.format "val x :\n \nint" ;
  protect_string @@ C.format "x + y * z" ;
  protect_string @@ C.format "let x = 4 in x" ;
  protect_string @@ C.format "sig end" ;
  protect_string
  @@ C.format
       "sig\n\n\
       \ val x : foo -> bar\n\
       \  (** this does something *)\n\n\
       \ val f : a -> b -> c ->\n\n\
       \ d     end" ;
  let some_function =
    {|
let ssmap
    :  (module MapT
          with type key = string
           and type data = string
           and type map = SSMap.map )
    -> unit
  =
  ()
|}
  in
  protect_string @@ C.format some_function ;
  protect_string @@ C.config [("profile", "janestreet")] ;
  protect_string @@ C.format some_function ;
  protect_unit @@ C.halt ()
