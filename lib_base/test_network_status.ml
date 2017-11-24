(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Not_running
  | Forking of {
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }
  | Running of {
      net_id: Net_id.t ;
      genesis: Block_hash.t ;
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }

let encoding =
  let open Data_encoding in
  union [
    case (Tag 0)
      (obj1 (req "status" (constant "not_running")))
      (function Not_running -> Some () | _ -> None)
      (fun () -> Not_running) ;
    case (Tag 1)
      (obj3
         (req "status" (constant "forking"))
         (req "protocol" Protocol_hash.encoding)
         (req "expiration" Time.encoding))
      (function
        | Forking { protocol ; expiration } ->
            Some ((), protocol, expiration)
        | _ -> None)
      (fun ((), protocol, expiration) ->
         Forking { protocol ; expiration }) ;
    case (Tag 2)
      (obj5
         (req "status" (constant "running"))
         (req "net_id" Net_id.encoding)
         (req "genesis" Block_hash.encoding)
         (req "protocol" Protocol_hash.encoding)
         (req "expiration" Time.encoding))
      (function
        | Running { net_id ; genesis ; protocol ; expiration } ->
            Some ((), net_id, genesis, protocol, expiration)
        | _ -> None)
      (fun ((), net_id, genesis, protocol, expiration) ->
         Running { net_id ; genesis ; protocol ; expiration }) ;
  ]

let pp ppf = function
  | Not_running -> Format.fprintf ppf "@[<v 2>Not running@]"
  | Forking { protocol ; expiration } ->
      Format.fprintf ppf
        "@[<v 2>Forking %a (expires %a)@]"
        Protocol_hash.pp
        protocol
        Time.pp_hum
        expiration
  | Running { net_id ; genesis ; protocol ; expiration } ->
      Format.fprintf ppf
        "@[<v 2>Running %a\
         @ Genesis: %a\
         @ Net id: %a\
         @ Expiration: %a@]"
        Protocol_hash.pp protocol
        Block_hash.pp genesis
        Net_id.pp net_id
        Time.pp_hum expiration
