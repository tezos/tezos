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
    ?(chain = `Main)
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
    ?src_pk
    block
    delegate =
  begin
    match src_sk with
    | None ->
        Client_keys.get_key cctxt delegate >>=? fun (_, _, src_sk) ->
        return src_sk
    | Some sk -> return sk
  end >>=? fun src_sk ->
  begin
    match src_pk with
    | None ->
        Client_keys.get_key cctxt delegate >>=? fun (_, src_pk, _) ->
        return src_pk
    | Some pk -> return pk
  end >>=? fun src_pk ->
  Alpha_services.Helpers.current_level
    cctxt ~offset:1l (chain, block) >>=? fun level ->
  let seed_nonce, seed_nonce_hash =
    if level.expected_commitment then
      let seed_nonce = Client_baking_forge.generate_seed_nonce () in
      let seed_nonce_hash = Nonce.hash seed_nonce in
      Some seed_nonce, Some seed_nonce_hash
    else
      None, None in
  Client_baking_forge.forge_block cctxt
    ?force
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ~await_endorsements
    ?timestamp:(if minimal_timestamp then None else Some (Time.now ()))
    ?seed_nonce_hash
    ?mempool
    ?context_path
    ~priority:(`Auto (delegate, max_priority))
    ~src_sk
    block >>=? fun block_hash ->
  let src_pkh = Signature.Public_key.hash src_pk in
  Client_baking_forge.State.record cctxt src_pkh level.level >>=? fun () ->
  begin match seed_nonce with
    | None -> return_unit
    | Some seed_nonce ->
        Client_baking_nonces.add cctxt block_hash seed_nonce
        |> trace_exn (Failure "Error while recording block")
  end >>=? fun () ->
  cctxt#message "Injected block %a" Block_hash.pp_short block_hash >>= fun () ->
  return_unit

let endorse_block cctxt delegate =
  Client_keys.get_key cctxt delegate >>=? fun (_src_name, src_pk, src_sk) ->
  Client_baking_endorsement.forge_endorsement cctxt
    cctxt#block ~src_sk src_pk >>=? fun oph ->
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

let do_reveal cctxt block blocks =
  let nonces = List.map snd blocks in
  Client_baking_revelation.forge_seed_nonce_revelation
    cctxt block nonces >>=? fun () ->
  return_unit

let reveal_block_nonces (cctxt : #Proto_alpha.full) block_hashes =
  cctxt#with_lock begin fun () ->
    Client_baking_nonces.load cctxt
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
      match Block_hash.Map.find_opt bi.hash nonces with
      | None ->
          cctxt#warning "Cannot find nonces for block %a (ignoring)@."
            Block_hash.pp_short bi.hash >>= fun () ->
          return_none
      | Some nonce ->
          return_some (bi.hash, (bi.level, nonce)))
    block_infos >>=? fun blocks ->
  do_reveal cctxt cctxt#block blocks

let reveal_nonces cctxt () =
  Client_baking_forge.get_unrevealed_nonces
    cctxt cctxt#block >>=? fun nonces ->
  do_reveal cctxt cctxt#block nonces >>=? fun () ->
  Client_baking_forge.filter_outdated_nonces cctxt cctxt#block >>=? fun () ->
  return_unit
