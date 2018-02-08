(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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

type (+'m,'pr,'p,'q,'i,'o, 'e) raw =
  ('m,'pr,'p,'q,'i,'o, 'e) Resto.MakeService(RPC_encoding).t
  constraint 'meth = [< meth ]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) t =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, unit) raw
  constraint 'meth = [< meth ]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, unit) raw
  constraint 'meth = [< meth ]

include (Resto.MakeService(RPC_encoding)
         : (module type of struct include Resto.MakeService(RPC_encoding) end
             with type (+'m,'pr,'p,'q,'i,'o, 'e) t := ('m,'pr,'p,'q,'i,'o, 'e) raw
              and type (+'m,'pr,'p,'q,'i,'o, 'e) service := ('m,'pr,'p,'q,'i,'o, 'e) raw)
        )

let get_service = get_service ~error:Data_encoding.empty
let post_service = post_service ~error:Data_encoding.empty
let delete_service = delete_service ~error:Data_encoding.empty
let patch_service = patch_service ~error:Data_encoding.empty
let put_service = put_service ~error:Data_encoding.empty
