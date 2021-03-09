# ocamlformat-rpc-lib

## How to handle a new version in a client

The best way to handle multiple versions in the client is to abstract the lib versions called by the client, the functions in your api, e.g.:
```ocaml
val feature_1 : a -> b (* can raise failure *)
val feature_2 : c -> d (* can raise failure *)
```
These functions can fail, feel free to catch them when it's best for you, or use result types.

when implementing these features that rely on the rpc lib we have to make sure the corresponding implementation is available in the client:
```ocaml
let feature_1 x =
 match get_client () with
  | (module Impl) -> Impl.feature_1 x

let feature_2 x =
 match get_client () with
  | (module Impl) -> Impl.feature_2 x
```

where `get_client` will decide which implementation to return depending on the version of the protocol decided between the server and the client:
```ocaml
let get_client () : (module Client.S) =
  match get_process () with
  | Ok p -> (
     match p.impl with
     | (module V1) -> (module Client.V1)
     | (module V2) -> (module Client.V2) )
  | Error (`Msg msg) -> failwith msg
```
Here `get_process` is defined in a way to know what kind of module `Ocamlformat_rpc_lib.V` to use, returned by `Ocamlformat_rpc_lib.pick_impl`.

`Client` is defined like this, exposing the 2 features functions we need, while V1 only implements `feature_1` and V2 implements both:

```ocaml
module Client = struct
  module type S = sig
    val feature_1 : a -> b

    val feature_2 : c -> d

    val halt : unit -> unit
  end

  module V1 = struct
    let query t =
      get_process () >>| fun p ->
      Ocamlformat_rpc_lib.V1.output p.output t;
      Ocamlformat_rpc_lib.V1.read_input p.input

    let feature_1 x = do_something x

    let feature_2 = failwith "not implemented"

    let halt () = todo
  end

  module V2 = struct
    let query t =
      get_process () >>| fun p ->
      Ocamlformat_rpc_lib.V2.output p.output t;
      Ocamlformat_rpc_lib.V2.read_input p.input

    let feature_1 x = do_something x

    let feature_2 x = do_something_else x

    let halt () = todo
  end
end
```

You can use the `query` function in each implementation to send the appropriate request to the server and match the corresponding answer received from the server.
