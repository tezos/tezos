(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
      chain_id: Chain_id.t ;
      genesis: Block_hash.t ;
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }

let encoding =
  let open Data_encoding in
  describe ~title:"Test chain status" @@
  union [
    case (Tag 0) ~name:"Not_running"
      (obj1 (req "status" (constant "not_running")))
      (function Not_running -> Some () | _ -> None)
      (fun () -> Not_running) ;
    case (Tag 1) ~name:"Forking"
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
    case (Tag 2) ~name:"Running"
      (obj5
         (req "status" (constant "running"))
         (req "chain_id" Chain_id.encoding)
         (req "genesis" Block_hash.encoding)
         (req "protocol" Protocol_hash.encoding)
         (req "expiration" Time.encoding))
      (function
        | Running { chain_id ; genesis ; protocol ; expiration } ->
            Some ((), chain_id, genesis, protocol, expiration)
        | _ -> None)
      (fun ((), chain_id, genesis, protocol, expiration) ->
         Running { chain_id ; genesis ; protocol ; expiration }) ;
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
  | Running { chain_id ; genesis ; protocol ; expiration } ->
      Format.fprintf ppf
        "@[<v 2>Running %a\
         @ Genesis: %a\
         @ Net id: %a\
         @ Expiration: %a@]"
        Protocol_hash.pp protocol
        Block_hash.pp genesis
        Chain_id.pp chain_id
        Time.pp_hum expiration
