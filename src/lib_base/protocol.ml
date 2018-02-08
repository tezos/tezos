(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let (//) = Filename.concat

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

let pp ppf op =
  Data_encoding.Json.pp ppf
    (Data_encoding.Json.construct encoding op)

let env_version_to_string = function
  | V1 -> "V1"

let pp_ocaml_component ppf { name ; interface ; implementation } =
  Format.fprintf ppf
    "@[{@[<v 1> name = %S ;@ interface = %a ;@ implementation = %S ;@]@ }@]"
    name
    (fun ppf -> function
       | None -> Format.fprintf ppf "None"
       | Some s -> Format.fprintf ppf "Some %S" s)
    interface
    implementation

let pp_ocaml ppf { expected_env ; components } =
  Format.fprintf ppf
    "@[{@[<v 1> expected_env = %s ;@ components = [@[<v>%a@]] ;@]@ }@]"
    (env_version_to_string expected_env)
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf " ;@ ")
       pp_ocaml_component)
    components

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

module Meta = struct

  type t = {
    hash: Protocol_hash.t option ;
    expected_env_version: env_version option ;
    modules: string list ;
  }

  let name = "TEZOS_PROTOCOL"

  let encoding =
    let open Data_encoding in
    conv
      (fun { hash ; expected_env_version ; modules } ->
         (hash, expected_env_version, modules))
      (fun (hash, expected_env_version, modules) ->
         { hash ; expected_env_version ; modules }) @@
    obj3
      (opt "hash"
         ~description:"Used to force the hash of the protocol"
         Protocol_hash.encoding)
      (opt "expected_env_version"
         env_version_encoding)
      (req "modules"
         ~description:"Modules comprising the protocol"
         (list string))

  let to_file ~dir:dirname ?hash ?env_version modules =
    let config_file =
      Data_encoding.Json.construct
        encoding
        { hash ; expected_env_version = env_version ; modules } in
    Utils.write_file ~bin:false (dirname // name) @@
    Data_encoding.Json.to_string config_file

  let of_file ~dir:dirname =
    Utils.read_file ~bin:false (dirname // name) |>
    Data_encoding.Json.from_string |> function
    | Error err -> Pervasives.failwith err
    | Ok json -> Data_encoding.Json.destruct encoding json

end

let find_component dirname module_name =
  let name_lowercase = String.uncapitalize_ascii module_name in
  let implementation = dirname // name_lowercase ^ ".ml" in
  let interface = implementation ^ "i" in
  match Sys.file_exists implementation, Sys.file_exists interface with
  | false, _ -> Pervasives.failwith @@ "Not such file: " ^ implementation
  | true, false ->
      let implementation = Utils.read_file ~bin:false implementation in
      { name = module_name; interface = None; implementation }
  | _ ->
      let interface = Utils.read_file ~bin:false interface in
      let implementation = Utils.read_file ~bin:false implementation in
      { name = module_name; interface = Some interface; implementation }

let read_dir dir =
  let meta = Meta.of_file ~dir in
  let components = List.map (find_component dir) meta.modules in
  let expected_env =
    match meta.expected_env_version with
    | None -> V1
    | Some v -> v in
  let protocol = { expected_env ; components } in
  let hash =
    match meta.hash with
    | None -> hash protocol
    | Some hash -> hash in
  hash, protocol

open Lwt.Infix

let create_files dir units =
  Lwt_utils_unix.remove_dir dir >>= fun () ->
  Lwt_utils_unix.create_dir dir >>= fun () ->
  Lwt_list.map_s
    (fun { name ; interface ; implementation } ->
       let name = String.lowercase_ascii name in
       let ml = dir // (name ^ ".ml") in
       let mli = dir // (name ^ ".mli") in
       Lwt_utils_unix.create_file ml implementation >>= fun () ->
       match interface with
       | None -> Lwt.return [ml]
       | Some content ->
           Lwt_utils_unix.create_file mli content >>= fun () ->
           Lwt.return [ mli ; ml ])
    units >>= fun files ->
  let files = List.concat files in
  Lwt.return files

let write_dir dir ?hash (p: t) =
  create_files dir p.components >>= fun _files ->
  Meta.to_file
    ~dir
    ?hash
    ~env_version:p.expected_env
    (List.map (fun { name ; _ } -> String.capitalize_ascii name) p.components) ;
  Lwt.return_unit

