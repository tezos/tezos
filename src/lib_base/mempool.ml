(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  known_valid: Operation_hash.t list ;
  pending: Operation_hash.Set.t ;
}
type mempool = t

let encoding =
  let open Data_encoding in
  conv
    (fun { known_valid ; pending } -> (known_valid, pending))
    (fun (known_valid, pending) -> { known_valid ; pending })
    (obj2
       (req "known_valid" (dynamic_size (list Operation_hash.encoding)))
       (req "pending" (dynamic_size Operation_hash.Set.encoding)))

let empty = {
  known_valid = [] ;
  pending = Operation_hash.Set.empty ;
}
