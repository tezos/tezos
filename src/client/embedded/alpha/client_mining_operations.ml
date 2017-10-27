(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519

open Operation

type operation = {
  hash: Operation_hash.t ;
  content: Operation.t option
}

let monitor cctxt ?contents ?check () =
  Client_node_rpcs.Operations.monitor cctxt ?contents () >>=? fun ops_stream ->
  let convert ops =
    Lwt.return ops >>=? fun ops ->
    map_s
      (fun (hash, op) ->
         match op with
         | None -> return { hash; content = None }
         | Some (op : Operation.raw) ->
             Client_proto_rpcs.Helpers.Parse.operations cctxt
               `Prevalidation ?check [op] >>=? function
             | [proto] ->
                 return { hash ; content = Some proto }
             | _ -> failwith "Error while parsing the operation")
     (List.concat  ops)
  in
  return (Lwt_stream.map_s convert ops_stream)


type valid_endorsement = {
  hash: Operation_hash.t ;
  source: public_key_hash ;
  block: Block_hash.t ;
  slots: int list ;
}

(*
let filter_valid_endorsement cctxt ({ hash ; content } : operation) =
  let open Tezos_context in
  match content with
  | None
  | Some { contents = Anonymous_operations _ }
  | Some { contents = Sourced_operations (Dictator_operation _ ) }
  | Some { contents = Sourced_operations (Manager_operations _ ) } ->
      Lwt.return_none
  | Some { shell = {net_id} ;
           contents =
             Sourced_operations (Delegate_operations { source ; operations }) } ->
      let source = Ed25519.Public_key.hash source in
      let endorsements =
        Utils.unopt_list @@ List.map
          (function
            | Endorsement { block ; slot } -> Some (block, slot)
            | _ -> None)
          operations in
      match endorsements with
      | [] -> Lwt.return_none
      | ((block, _) :: _) as slots ->
          try
            let slots =
              List.map
                (fun (block', slot) ->
                   if not (Block_hash.equal block block') then raise Not_found ;
                   slot)
                slots in
            (* Ensure thath the block has been previously validated by
               the node. This might took some times... *)
            Client_node_rpcs.validate_block cctxt net_id block >>= function
            | Error error ->
                lwt_log_info
                  "@[<v 2>Found endorsement for an invalid block@,%a@["
                  pp_print_error error >>= fun () ->
                Lwt.return_none
            | Ok () ->
                Client_node_rpcs.Blocks.preapply
                  cctxt (`Hash block) [Client_node_rpcs.Hash hash] >>= function
                | Ok _ ->
                    Lwt.return (Some { hash ; source ; block ; slots })
                | Error error ->
                    lwt_log_error
                      "@[<v 2>Error while prevalidating endorsements@,%a@["
                      pp_print_error error >>= fun () ->
                    Lwt.return_none
          with Not_found -> Lwt.return_none

let monitor_endorsement cctxt =
  monitor cctxt ~contents:true ~check:true () >>=? fun ops_stream ->
  let endorsement_stream, push = Lwt_stream.create () in
  Lwt.async begin fun () ->
    Lwt_stream.closed ops_stream >|= fun () -> push None
  end ;
  Lwt.async begin fun () ->
    Lwt_stream.iter_p
      (fun ops ->
         match ops with
         | Error _ as err ->
             push (Some err) ;
             Lwt.return_unit
         | Ok ops ->
             Lwt_list.iter_p
               (fun e ->
                  filter_valid_endorsement cctxt e >>= function
                  | None -> Lwt.return_unit
                  | Some e -> push (Some (Ok e)) ; Lwt.return_unit)
               ops)
      ops_stream
  end ;
  return endorsement_stream
*)

(* Temporary desactivate the monitoring of endorsement:
   too slow for now. *)
let monitor_endorsement _ =
  let stream, _push = Lwt_stream.create () in
  return stream

