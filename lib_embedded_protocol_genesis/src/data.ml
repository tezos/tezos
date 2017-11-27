(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Command = struct

  type t =
    (* Activate a protocol *)
    | Activate of {
        protocol: Protocol_hash.t ;
        validation_passes: int ;
      }

    (* Activate a protocol as a testnet *)
    | Activate_testnet of {
        protocol: Protocol_hash.t ;
        validation_passes: int ;
        delay: Int64.t ;
      }

  let mk_case name args =
    let open Data_encoding in
    conv
      (fun o -> ((), o))
      (fun ((), o) -> o)
      (merge_objs
         (obj1 (req "command" (constant name)))
         args)

  let encoding =
    let open Data_encoding in
    union ~tag_size:`Uint8 [
      case ~tag:0
        (mk_case "activate"
           (obj2
              (req "hash" Protocol_hash.encoding)
              (req "validation_passes" uint8)
           ))
        (function
          | Activate { protocol ; validation_passes } ->
              Some (protocol, validation_passes)
          | _ -> None)
        (fun (protocol, validation_passes) ->
           Activate { protocol ; validation_passes }) ;
      case ~tag:1
        (mk_case "activate_testnet"
           (obj3
              (req "hash" Protocol_hash.encoding)
              (req "validation_passes" uint8)
              (req "validity_time" int64)))
        (function
          | Activate_testnet { protocol ; validation_passes ; delay } ->
              Some (protocol, validation_passes, delay)
          | _ -> None)
        (fun (protocol, validation_passes, delay) ->
           Activate_testnet { protocol ; validation_passes ; delay }) ;
    ]

  let signed_encoding =
    let open Data_encoding in
    obj2
      (req "content" encoding)
      (req "signature" Ed25519.Signature.encoding)

  let forge shell command =
    Data_encoding.Binary.to_bytes
      (Data_encoding.tup2 Block_header.shell_header_encoding encoding)
      (shell, command)

end

module Pubkey = struct

  let pubkey_key = ["genesis_key"]

  let default =
    let pubkey =
      "4d5373455738070434f214826d301a1c206780d7f789fcbf94c2149b2e0718cc" in
    Ed25519.Public_key.of_bytes
      (Bytes.of_string (Hex_encode.hex_decode pubkey))

  let get_pubkey ctxt =
    Context.get ctxt pubkey_key >>= function
    | None -> Lwt.return default
    | Some b ->
        match Data_encoding.Binary.of_bytes Ed25519.Public_key.encoding b with
        | None -> Lwt.return default
        | Some pk -> Lwt.return pk

  let set_pubkey ctxt v =
    Context.set ctxt pubkey_key @@
    Data_encoding.Binary.to_bytes Ed25519.Public_key.encoding v

  let sandbox_encoding =
    let open Data_encoding in
    merge_objs
      (obj1 (req "genesis_pubkey" Ed25519.Public_key.encoding))
      Data_encoding.unit

  let may_change_default ctxt json =
    match Data_encoding.Json.destruct sandbox_encoding json with
    | exception _ ->
        Lwt.return ctxt
    | (pubkey, ()) ->
        set_pubkey ctxt pubkey >>= fun ctxt ->
        Lwt.return ctxt

end

module Init = struct

  type error += Incompatible_protocol_version

  let version_key = ["version"]

  (* This key should always be populated for every version of the
     protocol.  It's absence meaning that the context is empty. *)
  let version_value = "genesis"

  let may_initialize ctxt =
    Context.get ctxt version_key >>= function
    | None ->
        Context.set
          ctxt version_key (MBytes.of_string version_value) >>= fun ctxt ->
        return ctxt
    | Some bytes ->
        let s = MBytes.to_string bytes in
        fail_unless Compare.String.(s = version_value)
          Incompatible_protocol_version >>=? fun () ->
        return ctxt

  let sandboxed_key = [ "v1" ; "sandboxed" ]

  let set_sandboxed ctxt json =
    Context.set ctxt sandboxed_key
      (Data_encoding.Binary.to_bytes Data_encoding.json json)
  let get_sandboxed ctxt =
    Context.get ctxt sandboxed_key >>= fun b ->
    match b with
    | None -> return None
    | Some b ->
        return (Data_encoding.Binary.of_bytes Data_encoding.json b)

  type error += Unimplemented_sandbox_migration

  let configure_sandbox ctxt json =
    let json =
      match json with
      | None -> `O []
      | Some json -> json in
    Context.get ctxt version_key >>= function
    | None ->
        set_sandboxed ctxt json >>= fun ctxt ->
        Pubkey.may_change_default ctxt json >>= fun ctxt ->
        return ctxt
    | Some _ ->
        get_sandboxed ctxt >>=? function
        | None ->
            fail Unimplemented_sandbox_migration
        | Some _ ->
            (* FIXME GRGR fail if parameter changed! *)
            (* failwith "Changing sandbox parameter is not yet implemented" *)
            return ctxt

end
