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

let await_bootstrapped_node (cctxt: #Proto_alpha.full) =
  (* Waiting for the node to be synchronized *)
  cctxt#message "Waiting for the node to be synchronized with its \
                 peers..." >>= fun () ->
  Shell_services.Monitor.bootstrapped cctxt >>=? fun _ ->
  cctxt#message "Node synchronized." >>= fun () ->
  return_unit

let monitor_fork_testchain (cctxt: #Proto_alpha.full) ~cleanup_nonces  =
  (* Waiting for the node to be synchronized *)
  cctxt#message "Waiting for the test chain to be forked..." >>= fun () ->
  Shell_services.Monitor.active_chains cctxt >>=? fun (stream, _) ->
  let rec loop () =
    Lwt_stream.next stream >>= fun l ->
    let testchain =
      List.find_opt (function
          | Shell_services.Monitor.Active_test _ -> true
          | _ -> false) l in
    match testchain with
    | Some (Active_test { protocol ; expiration_date })
      when Protocol_hash.equal Proto_alpha.hash protocol -> begin
        Chain_services.chain_id cctxt ~chain:`Test () >>=? fun test_chain_id ->
        let abort_daemon () =
          cctxt#message "Test chain's expiration date reached \
                         (%a)... Stopping the daemon.@."
            Time.pp_hum expiration_date >>= fun () ->
          if cleanup_nonces then
            (* Clean-up existing nonces *)
            cctxt#with_lock begin fun () ->
              Client_baking_nonces.load cctxt >>=? fun nonces ->
              let nonces = Client_baking_nonces.remove_all nonces test_chain_id in
              Client_baking_nonces.save cctxt nonces
            end
          else
            return_unit >>=? fun () ->
            exit 0 in
        let canceler = Lwt_canceler.create () in
        Lwt_canceler.on_cancel canceler (fun () -> abort_daemon () >>= function _ -> Lwt.return_unit) ;
        let delay = Int64.to_int Time.(diff expiration_date (now ())) in
        if delay <= 0 then
          (* Testchain already expired... Retrying. *)
          loop ()
        else
          let timeout =
            Lwt_timeout.create delay (fun () -> Lwt_canceler.cancel canceler |> ignore) in
          Lwt_timeout.start timeout ;
          return_unit
      end
    | None -> loop ()
    | Some _ -> loop () (* Got a testchain for a different protocol, skipping *) in
  loop () >>=? fun () ->
  cctxt#message "Test chain forked." >>= fun () ->
  return_unit

module Endorser = struct

  let run (cctxt : #Proto_alpha.full) ~chain ~delay delegates =
    Daemon_state.upgrade_files cctxt () >>=? fun () ->
    await_bootstrapped_node cctxt >>=? fun _ ->
    begin if chain = `Test then
        monitor_fork_testchain cctxt ~cleanup_nonces:false
      else
        return_unit end >>=? fun () ->
    Client_baking_blocks.monitor_heads
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt chain >>=? fun block_stream ->
    cctxt#message "Endorser started." >>= fun () ->
    Client_baking_endorsement.create cctxt
      ~delay
      delegates
      block_stream >>=? fun () ->
    return_unit

end

module Baker = struct

  let run
      (cctxt : #Proto_alpha.full)
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?await_endorsements
      ?max_priority
      ~chain
      ~context_path
      delegates =
    Daemon_state.upgrade_files cctxt () >>=? fun () ->
    await_bootstrapped_node cctxt >>=? fun _ ->
    begin if chain = `Test then
        monitor_fork_testchain cctxt ~cleanup_nonces:true
      else
        return_unit end >>=? fun () ->
    Client_baking_blocks.monitor_heads
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt chain >>=? fun block_stream ->
    cctxt#message "Baker started." >>= fun () ->
    Client_baking_forge.create cctxt
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?await_endorsements
      ?max_priority
      ~context_path
      delegates
      block_stream >>=? fun () ->
    return_unit

end

module Accuser = struct

  let run (cctxt : #Proto_alpha.full) ~chains ~preserved_levels =
    await_bootstrapped_node cctxt >>=? fun _ ->
    Client_baking_blocks.monitor_valid_blocks
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt ~chains () >>=? fun valid_blocks_stream ->
    cctxt#message "Accuser started." >>= fun () ->
    Client_baking_denunciation.create cctxt ~preserved_levels valid_blocks_stream >>=? fun () ->
    return_unit

end
