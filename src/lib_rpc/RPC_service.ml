(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]

let string_of_meth = Resto.string_of_meth
let meth_of_string = Resto.meth_of_string

let meth_encoding =
  let open Data_encoding in
  conv
    string_of_meth
    (fun m ->
       match meth_of_string m with
       | None -> Pervasives.failwith "Cannot parse methods"
       | Some s -> s)
    string

module MethMap = Resto.MethMap

include Resto.MakeService(RPC_encoding)
