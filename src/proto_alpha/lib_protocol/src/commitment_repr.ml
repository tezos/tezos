(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  blinded_public_key_hash : Blinded_public_key_hash.t ;
  amount : Tez_repr.t
}

let encoding =
  let open Data_encoding in
  conv
    (fun { blinded_public_key_hash ; amount } ->
       ( blinded_public_key_hash, amount ))
    (fun ( blinded_public_key_hash, amount) ->
       { blinded_public_key_hash ; amount })
    (obj2
       (req "blinded_public_key_hash" Blinded_public_key_hash.encoding)
       (req "amount" Tez_repr.encoding)
    )
