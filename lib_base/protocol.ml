(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  expected_env: env_version ;
  components: component list ;
}

and component = {
  name: string ;
  interface: string option ;
  implementation: string ;
}

and env_version = V1

let component_encoding =
  let open Data_encoding in
  conv
    (fun { name ; interface; implementation } ->
       (name, interface, implementation))
    (fun (name, interface, implementation) ->
       { name ; interface ; implementation })
    (obj3
       (req "name" string)
       (opt "interface" string)
       (req "implementation" string))

let env_version_encoding =
  let open Data_encoding in
  conv
    (function V1 -> 0)
    (function 0 -> V1 | _ -> failwith "unexpected environment version")
    int16

let encoding =
  let open Data_encoding in
  conv
    (fun { expected_env ; components } -> (expected_env, components))
    (fun (expected_env, components) -> { expected_env ; components })
    (obj2
       (req "expected_env_version" env_version_encoding)
       (req "components" (list component_encoding)))

let pp fmt op =
  Format.pp_print_string fmt @@
  Data_encoding_ezjsonm.to_string (Data_encoding.Json.construct encoding op)

let compare = Pervasives.compare
let equal = (=)

let (=) = equal
let (<>) x y = compare x y <> 0
let (<) x y = compare x y < 0
let (<=) x y = compare x y <= 0
let (>=) x y = compare x y >= 0
let (>) x y = compare x y > 0
let min x y = if x <= y then x else y
let max x y = if x <= y then y else x

let to_bytes v = Data_encoding.Binary.to_bytes encoding v
let of_bytes b = Data_encoding.Binary.of_bytes encoding b
let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b
let hash proto = Protocol_hash.hash_bytes [to_bytes proto]
let hash_raw proto = Protocol_hash.hash_bytes [proto]
