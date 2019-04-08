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

open Proto_alpha
open Alpha_context

let bake_block
    (cctxt : #Proto_alpha.full)
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?(await_endorsements = false)
    ?force
    ?max_priority
    ?(minimal_timestamp = false)
    ?mempool
    ?context_path
    ?src_sk
    ~chain
    ~head
    delegate =
  begin
    match src_sk with
    | None ->
        Client_keys.get_key cctxt delegate >>=? fun (_, _, src_sk) ->
        return src_sk
    | Some sk -> return sk
  end >>=? fun src_sk ->
  Alpha_services.Helpers.current_level
    cctxt ~offset:1l (chain, head) >>=? fun level ->
  let seed_nonce, seed_nonce_hash =
    if level.expected_commitment then
      let seed_nonce = Client_baking_forge.generate_seed_nonce () in
      let seed_nonce_hash = Nonce.hash seed_nonce in
      Some seed_nonce, Some seed_nonce_hash
    else
      None, None in
  let timestamp =
    if minimal_timestamp then
      None
    else
      Some Time.System.(to_protocol (Systime_os.now ())) in
  Client_baking_forge.forge_block cctxt
    ?force
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ~await_endorsements
    ?timestamp
    ?seed_nonce_hash
    ?mempool
    ?context_path
    ~chain
    ~priority:(`Auto (delegate, max_priority))
    ~delegate_pkh:delegate
    ~delegate_sk:src_sk
    head >>=? fun block_hash ->
  begin match seed_nonce with
    | None -> return_unit
    | Some seed_nonce ->
        cctxt#with_lock begin fun () ->
          let open Client_baking_nonces in
          Client_baking_files.resolve_location cctxt ~chain `Nonce >>=? fun nonces_location ->
          load cctxt nonces_location >>=? fun nonces ->
          let nonces = add nonces block_hash seed_nonce in
          save cctxt nonces_location nonces
        end |> trace_exn (Failure "Error while recording block")
  end >>=? fun () ->
  cctxt#message "Injected block %a" Block_hash.pp_short block_hash >>= fun () ->
  return_unit

let endorse_block cctxt ~chain delegate =
  Client_keys.get_key cctxt delegate >>=? fun (_src_name, src_pk, src_sk) ->
  Client_baking_endorsement.forge_endorsement cctxt
    ~chain ~block:cctxt#block
    ~src_sk src_pk >>=? fun oph ->
  cctxt#answer "Operation successfully injected in the node." >>= fun () ->
  cctxt#answer "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return_unit

let get_predecessor_cycle (cctxt : #Client_context.printer) cycle =
  match Cycle.pred cycle with
  | None ->
      if Cycle.(cycle = root) then
        cctxt#error "No predecessor for the first cycle"
      else
        cctxt#error
          "Cannot compute the predecessor of cycle %a"
          Cycle.pp cycle
  | Some cycle -> Lwt.return cycle

let do_reveal cctxt ~chain ~block nonces =
  Client_baking_revelation.inject_seed_nonce_revelation
    cctxt ~chain ~block nonces >>=? fun () ->
  return_unit

let reveal_block_nonces (cctxt : #Proto_alpha.full) ~chain ~block block_hashes =
  cctxt#with_lock begin fun () ->
    Client_baking_files.resolve_location cctxt ~chain `Nonce >>=? fun nonces_location ->
    Client_baking_nonces.load cctxt nonces_location
  end >>=? fun nonces ->
  Lwt_list.filter_map_p
    (fun hash ->
       Lwt.catch
         (fun () ->
            Client_baking_blocks.info cctxt (`Hash (hash, 0)) >>= function
            | Ok bi -> Lwt.return_some bi
            | Error _ ->
                Lwt.fail Not_found)
         (fun _ ->
            cctxt#warning
              "Cannot find block %a in the chain. (ignoring)@."
              Block_hash.pp_short hash >>= fun () ->
            Lwt.return_none))
    block_hashes >>= fun block_infos ->
  filter_map_s (fun (bi : Client_baking_blocks.block_info) ->
      match Client_baking_nonces.find_opt nonces bi.hash with
      | None ->
          cctxt#warning "Cannot find nonces for block %a (ignoring)@."
            Block_hash.pp_short bi.hash >>= fun () ->
          return_none
      | Some nonce ->
          return_some (bi.hash, (bi.level, nonce)))
    block_infos >>=? fun nonces ->
  let nonces = List.map snd nonces in
  do_reveal cctxt ~chain ~block nonces

let reveal_nonces (cctxt : #Proto_alpha.full) ~chain ~block () =
  let open Client_baking_nonces in
  cctxt#with_lock begin fun () ->
    Client_baking_files.resolve_location cctxt ~chain `Nonce >>=? fun nonces_location ->
    load cctxt nonces_location >>=? fun nonces ->
    get_unrevealed_nonces cctxt nonces_location nonces >>=? fun nonces_to_reveal ->
    do_reveal cctxt ~chain ~block nonces_to_reveal >>=? fun () ->
    filter_outdated_nonces cctxt nonces_location nonces >>=? fun nonces ->
    save cctxt nonces_location nonces >>=? fun () ->
    return_unit
  end
