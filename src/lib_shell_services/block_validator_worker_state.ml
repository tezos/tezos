(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Request = struct
  type view = {
    net_id : Net_id.t ;
    block : Block_hash.t ;
    peer : P2p_types.Peer_id.t option ;
  }
  let encoding =
    let open Data_encoding in
    conv
      (fun { net_id ; block ; peer } -> (block, net_id, peer))
      (fun (block, net_id, peer) -> { net_id ; block ; peer })
      (obj3
         (req "block" Block_hash.encoding)
         (req "net_id" Net_id.encoding)
         (opt "peer" P2p_types.Peer_id.encoding))

  let pp ppf { net_id ; block ; peer } =
    Format.fprintf ppf "Validation of %a (net: %a)"
      Block_hash.pp block
      Net_id.pp_short net_id ;
    match peer with
    | None -> ()
    | Some peer ->
        Format.fprintf ppf "from peer %a"
          P2p_types.Peer_id.pp_short peer
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

  let encoding error_encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          (obj1 (req "message" string))
          (function Debug msg -> Some msg | _ -> None)
          (fun msg -> Debug msg) ;
        case (Tag 1)
          (obj2
             (req "successful_validation" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Validation_success (r, s) -> Some (r, s) | _ -> None)
          (fun (r, s) -> Validation_success (r, s)) ;
        case (Tag 2)
          (obj3
             (req "failed_validation" Request.encoding)
             (req "status" Worker_types.request_status_encoding)
             (dft "errors" error_encoding []))
          (function Validation_failure (r, s, err) -> Some (r, s, err) | _ -> None)
          (fun (r, s, err) -> Validation_failure (r, s, err)) ]

  let pp ppf = function
    | Debug msg -> Format.fprintf ppf "%s" msg
    | Validation_success (req, { pushed ; treated ; completed }) ->
        Format.fprintf ppf
          "@[<v 2>Block %a succesfully validated@,\
           Pushed: %a, Treated: %a, Completed: %a@]"
          Block_hash.pp req.block
          Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
    | Validation_failure (req, { pushed ; treated ; completed }, errs)->
        Format.fprintf ppf
          "@[<v 2>Validation of block %a failed@,\
           Pushed: %a, Treated: %a, Completed: %a@,\
           Error: %a@]"
          Block_hash.pp req.block
          Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
          Error_monad.pp_print_error errs
end

module Worker_state = struct
  type view = unit
  let encoding = Data_encoding.empty
  let pp _ppf _view = ()
end
