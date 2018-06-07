(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  disable_mempool : bool ;
  private_node : bool ;
}

let encoding =
  let open Data_encoding in
  (conv
     (fun { disable_mempool ; private_node } ->
        disable_mempool , private_node)
     (fun (disable_mempool , private_node) ->
        { disable_mempool ; private_node }))
    (obj2
       (req "disable_mempool" bool)
       (req "private_node" bool))

let pp _ppf _ = ()
