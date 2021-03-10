module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e
  end
end

open Result.Infix
module Ocf = Ocamlformat_rpc_lib

let current_client : Ocf.client option ref = ref None

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
  let input = Unix.in_channel_of_descr out_ in
  let output = Unix.out_channel_of_descr in_ in
  log "[ocf] proposed versions: @[<hv>%a@]\n%!"
    (Format.pp_print_list
       ~pp_sep:(fun fs () -> Format.fprintf fs ",@ ")
       Format.pp_print_string )
    supported_versions ;
  Ocf.pick_client ~pid input output supported_versions
  >>| fun client ->
  (match client with `V1 _ -> log "[ocf] client V1 selected\n%!") ;
  current_client := Some client ;
  client

let get_client () =
  match !current_client with
  | None -> start ()
  | Some cl ->
      let i, _ = Unix.waitpid [WNOHANG] (Ocf.pid cl) in
      if i = 0 then Ok cl else start ()

let config c =
  get_client () >>= fun cl -> log "[ocf] Config\n%!" ; Ocf.config c cl

let format x =
  get_client ()
  >>= fun cl ->
  log "[ocf] Format '%s'\n%!" x ;
  Ocf.format x cl

let halt () =
  get_client ()
  >>= fun cl ->
  log "[ocf] Halt\n%!" ;
  Ocf.halt cl >>| fun () -> current_client := None

let protect_unit x =
  match x with Ok () -> () | Error (`Msg e) -> log "Error: %s\n%!" e

let protect_string x =
  match x with
  | Ok s -> log "@[<hv>Output:@;%s@]\n%!" s
  | Error (`Msg e) -> log "Error: %s\n%!" e

let () =
  log "Starting then doing nothing\n%!" ;
  protect_unit @@ halt ()

let () =
  log "Sending Type requests\n%!" ;
  protect_string @@ format "char -> string" ;
  protect_string @@ format "int -> int" ;
  protect_string @@ format " int    (* foo *) \n\n ->     int  (* bar *)" ;
  protect_unit @@ config [("foo", "bar")] ;
  protect_unit @@ config [("margin", "10")] ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_unit @@ config [("margin", "80")] ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ format "val x :\n \nint" ;
  protect_string @@ format "x + y * z" ;
  protect_string @@ format "let x = 4 in x" ;
  protect_string @@ format "sig end" ;
  protect_string
  @@ format
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
  protect_string @@ format some_function ;
  protect_unit @@ config [("profile", "janestreet")] ;
  protect_string @@ format some_function ;
  protect_unit @@ halt ()
