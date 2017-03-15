(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Ed25519

type error += Inconsistent_hash of Public_key.t * Public_key_hash.t * Public_key_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"public_key.inconsistent_hash"
    ~title:"Inconsistent public key hash"
    ~description:"A revealed public key is inconsistent with the announced hash"
    ~pp:(fun ppf (k, eh, ph) ->
        Format.fprintf ppf "Hash of public key %s is not %a as announced but %a"
          (Public_key.to_b58check k)
          Public_key_hash.pp ph
          Public_key_hash.pp eh)
    Data_encoding.(obj3
                     (req "public_key" Public_key.encoding)
                     (req "expected_hash" Public_key_hash.encoding)
                     (req "provided_hash" Public_key_hash.encoding))
    (function Inconsistent_hash (k, eh, ph)   -> Some (k, eh, ph) | _ -> None)
    (fun (k, eh, ph) -> Inconsistent_hash (k, eh, ph))

let get = Storage.Public_key.get

let get_option = Storage.Public_key.get_option

let reveal c hash key =
  let actual_hash = Ed25519.Public_key.hash key in
  if Ed25519.Public_key_hash.equal hash actual_hash then
    Storage.Public_key.init_set c hash key
  else
    fail (Inconsistent_hash (key, actual_hash, hash))

let remove = Storage.Public_key.remove

let list ctxt =
  Storage.Public_key.fold ctxt [] ~f:(fun pk_h pk acc ->
      Lwt.return @@ (pk_h, pk) :: acc) >>= fun res ->
  return res
