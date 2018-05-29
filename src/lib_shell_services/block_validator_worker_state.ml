(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Request = struct
  type view = {
    chain_id : Chain_id.t ;
    block : Block_hash.t ;
    peer : P2p_peer.Id.t option ;
  }
  let encoding =
    let open Data_encoding in
    conv
      (fun { chain_id ; block ; peer } -> (block, chain_id, peer))
      (fun (block, chain_id, peer) -> { chain_id ; block ; peer })
      (obj3
         (req "block" Block_hash.encoding)
         (req "chain_id" Chain_id.encoding)
         (opt "peer" P2p_peer.Id.encoding))

  let pp ppf { chain_id ; block ; peer } =
    Format.fprintf ppf "Validation of %a (chain: %a)"
      Block_hash.pp block
      Chain_id.pp_short chain_id ;
    match peer with
    | None -> ()
    | Some peer ->
        Format.fprintf ppf "from peer %a"
          P2p_peer.Id.pp_short peer
end

module Event = struct
  type t =
    | Validation_success of Request.view * Worker_types.request_status
    | Validation_failure of Request.view * Worker_types.request_status * error list
    | Debug of string

  let level req =
    match req with
    | Debug _ -> Logging.Debug
    | Validation_success _
    | Validation_failure _ -> Logging.Notice

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0) ~name:"Debug"
          (obj1 (req "message" string))
          (function Debug msg -> Some msg | _ -> None)
          (fun msg -> Debug msg) ;
        case (Tag 1) ~name:"Validation_success"
          (obj2
             (req "successful_validation" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Validation_success (r, s) -> Some (r, s) | _ -> None)
          (fun (r, s) -> Validation_success (r, s)) ;
        case (Tag 2) ~name:"Validation_failure"
          (obj3
             (req "failed_validation" Request.encoding)
             (req "status" Worker_types.request_status_encoding)
             (dft "errors" RPC_error.encoding []))
          (function Validation_failure (r, s, err) -> Some (r, s, err) | _ -> None)
          (fun (r, s, err) -> Validation_failure (r, s, err)) ]

  let pp ppf = function
    | Debug msg -> Format.fprintf ppf "%s" msg
    | Validation_success (req, { pushed ; treated ; completed }) ->
        Format.fprintf ppf
          "@[<v 0>Block %a succesfully validated@,\
           Pushed: %a, Treated: %a, Completed: %a@]"
          Block_hash.pp req.block
          Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
    | Validation_failure (req, { pushed ; treated ; completed }, errs)->
        Format.fprintf ppf
          "@[<v 0>Validation of block %a failed@,\
           Pushed: %a, Treated: %a, Failed: %a@,\
           %a@]"
          Block_hash.pp req.block
          Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
          (Format.pp_print_list Error_monad.pp) errs
end

module Worker_state = struct
  type view = unit
  let encoding = Data_encoding.empty
  let pp _ppf _view = ()
end
