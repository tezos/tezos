(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Request = struct
  type view = Block_hash.t

  let encoding = Block_hash.encoding
  let pp = Block_hash.pp
end

module Event = struct
  type update =
    | Ignored_head
    | Branch_switch
    | Head_incrememt
  type t =
    | Processed_block of
        { request : Request.view ;
          request_status : Worker_types.request_status ;
          update : update ;
          fitness : Fitness.t }
    | Could_not_switch_testchain of error list

  let level = function
    | Processed_block req ->
        begin match req.update with
          | Ignored_head -> Internal_event.Info
          | Branch_switch | Head_incrememt -> Internal_event.Notice
        end
    | Could_not_switch_testchain _ -> Internal_event.Error

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Processed_block"
          (obj4
             (req "request" Request.encoding)
             (req "status" Worker_types.request_status_encoding)
             (req "outcome"
                (string_enum [ "ignored", Ignored_head ;
                               "branch", Branch_switch ;
                               "increment", Head_incrememt ]))
             (req "fitness" Fitness.encoding))
          (function
            | Processed_block { request ; request_status ; update ; fitness } ->
                Some (request, request_status, update, fitness)
            | _ -> None)
          (fun (request, request_status, update, fitness) ->
             Processed_block { request ; request_status ; update ; fitness }) ;
        case (Tag 1)
          ~title:"Could_not_switch_testchain"
          RPC_error.encoding
          (function
            | Could_not_switch_testchain err -> Some err
            | _ -> None)
          (fun err -> Could_not_switch_testchain err) ]

  let pp ppf = function
    | Processed_block req ->
        Format.fprintf ppf "@[<v 0>" ;
        begin match req.update with
          | Ignored_head ->
              Format.fprintf ppf
                "Current head is better than %a (fitness %a), we do not switch@,"
          | Branch_switch ->
              Format.fprintf ppf
                "Update current head to %a (fitness %a), changing branch@,"
          | Head_incrememt ->
              Format.fprintf ppf
                "Update current head to %a (fitness %a), same branch@,"
        end
          Request.pp req.request
          Fitness.pp req.fitness ;
        Format.fprintf ppf
          "Pushed: %a, Treated: %a, Completed: %a@]"
          Time.System.pp_hum req.request_status.pushed
          Time.System.pp_hum req.request_status.treated
          Time.System.pp_hum req.request_status.completed
    | Could_not_switch_testchain err ->
        Format.fprintf ppf "@[<v 0>Error while switching test chain:@ %a@]"
          (Format.pp_print_list Error_monad.pp) err

end

module Worker_state = struct
  type view =
    { active_peers : P2p_peer.Id.t list ;
      bootstrapped_peers : P2p_peer.Id.t list ;
      bootstrapped : bool }
  let encoding =
    let open Data_encoding in
    conv
      (fun { bootstrapped ; bootstrapped_peers ; active_peers } ->
         (bootstrapped, bootstrapped_peers, active_peers))
      (fun (bootstrapped, bootstrapped_peers, active_peers) ->
         { bootstrapped ; bootstrapped_peers ; active_peers })
      (obj3
         (req "bootstrapped" bool)
         (req "bootstrapped_peers" (list P2p_peer.Id.encoding))
         (req "active_peers" (list P2p_peer.Id.encoding)))

  let pp ppf { bootstrapped ; bootstrapped_peers ; active_peers } =
    Format.fprintf ppf
      "@[<v 0>Network is%s bootstrapped.@,\
       @[<v 2>Active peers:%a@]@,\
       @[<v 2>Bootstrapped peers:%a@]@]"
      (if bootstrapped then "" else " not yet")
      (fun ppf -> List.iter (Format.fprintf ppf "@,- %a" P2p_peer.Id.pp))
      active_peers
      (fun ppf -> List.iter (Format.fprintf ppf "@,- %a" P2p_peer.Id.pp))
      bootstrapped_peers
end
