(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
        fitness: Fitness.t ;
      }

    (* Activate a protocol as a testchain *)
    | Activate_testchain of {
        protocol: Protocol_hash.t ;
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
      case (Tag 0)
        (mk_case "activate"
           (obj2
              (req "hash" Protocol_hash.encoding)
              (req "fitness" Fitness.encoding)
           ))
        (function
          | Activate { protocol ; fitness} ->
              Some (protocol, fitness)
          | _ -> None)
        (fun (protocol, fitness) ->
           Activate { protocol ; fitness }) ;
      case (Tag 1)
        (mk_case "activate_testchain"
           (obj2
              (req "hash" Protocol_hash.encoding)
              (req "validity_time" int64)))
        (function
          | Activate_testchain { protocol ; delay } ->
              Some (protocol, delay)
          | _ -> None)
        (fun (protocol, delay) ->
           Activate_testchain { protocol ; delay }) ;
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
    Ed25519.Public_key.of_hex_exn (`Hex pubkey)

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

  let check_inited ctxt =
    Context.get ctxt version_key >>= function
    | None -> failwith "Internal error: uninitialized context."
    | Some version ->
        if Compare.String.(version_value <> MBytes.to_string version) then
          failwith "Internal error: incompatible protocol version" ;
        return ()

  let tag_first_block ctxt =
    Context.get ctxt version_key >>= function
    | None ->
        Context.set
          ctxt version_key (MBytes.of_string version_value) >>= fun ctxt ->
        return ctxt
    | Some _version ->
        failwith "Internal error: previously initialized context." ;

end
