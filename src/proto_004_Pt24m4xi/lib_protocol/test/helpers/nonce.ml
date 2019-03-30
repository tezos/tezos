(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc.< contact@tezos.com >                 *)
(*                                                                        *)
(*    All rights reserved.No warranty, explicit or implicit, provided.    *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

module Table = Hashtbl.Make(struct
    type t = Nonce_hash.t
    let hash h =
      Int32.to_int (MBytes.get_int32 (Nonce_hash.to_bytes h) 0)
    let equal = Nonce_hash.equal
  end)

let known_nonces = Table.create 17

let generate () =
  match
    Alpha_context.Nonce.of_bytes @@
    Rand.generate Alpha_context.Constants.nonce_length
  with
  | Ok nonce ->
      let hash = Alpha_context.Nonce.hash nonce in
      Table.add known_nonces hash nonce ;
      (hash, nonce)
  | Error _ -> assert false

let forget_all () = Table.clear known_nonces
let get hash = Table.find known_nonces hash
