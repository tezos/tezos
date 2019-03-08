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

type error += Invalid_chain_id of Chain_id.t
type error += Level_previously_endorsed of Raw_level.t
type error += Level_previously_baked of Raw_level.t

let () = begin
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id: "Daemon_state.invalid_chain_id"
    ~title: "Invalid chain id"
    ~description: "Requesting to register a highwatermark for neither main or test chain."
    ~pp:(fun ppf chain_id ->
        Format.fprintf ppf "Invalid chain id : %a is neither the main chain nor the test chain."
          Chain_id.pp chain_id)
    (obj1 (req "chain_id" Chain_id.encoding))
    (function
      | Invalid_chain_id chain_id -> Some chain_id
      | _ -> None)
    (fun chain_id -> Invalid_chain_id chain_id) ;
  register_error_kind
    `Permanent
    ~id:"Daemon_state_forge.block_already_baked"
    ~title: "Block already baked"
    ~description: "Trying to bake a block for a level that was \
                   previously done"
    ~pp:(fun ppf level ->
        Format.fprintf ppf "Level %a previously baked "
          Raw_level.pp level)
    (obj1 (req "level" Raw_level.encoding))
    (function
      | Level_previously_baked level -> Some level
      | _ -> None)
    (fun level -> Level_previously_baked level) ;
  register_error_kind
    `Permanent
    ~id:"Daemon_state_forge.block_already_endorsed"
    ~title: "Fail to preapply an operation"
    ~description: "Trying to endorse a block for a level that was \
                   previously done"
    ~pp:(fun ppf level ->
        Format.fprintf ppf "Level %a previously endorsed "
          Raw_level.pp level)
    (obj1 (req "level" Raw_level.encoding))
    (function
      | Level_previously_endorsed level -> Some level
      | _ -> None)
    (fun level -> Level_previously_endorsed level)
end

let name = "highwatermark"

type highwatermarks =
  { block_highwatermark : Raw_level.t Signature.Public_key_hash.Map.t ;
    endorsement_highwatermark : Raw_level.t Signature.Public_key_hash.Map.t }

type t =
  { main_chain : highwatermarks ;
    test_chain : highwatermarks }

let encoding =
  let open Data_encoding in
  let highwatermarks_encoding =
    obj2
      (req "highest_level_recorded_block" (Signature.Public_key_hash.Map.encoding Raw_level.encoding))
      (req "highest_level_recorded_endorsement"
         (Signature.Public_key_hash.Map.encoding Raw_level.encoding)) in
  conv
    (fun { main_chain = { block_highwatermark = main_block ;
                          endorsement_highwatermark = main_endorsement } ;
           test_chain = { block_highwatermark = test_block ;
                          endorsement_highwatermark = test_endorsement } } ->
      (main_block, main_endorsement), (test_block, test_endorsement)
    )
    (fun ((main_block, main_endorsement), (test_block, test_endorsement)) ->
       { main_chain = { block_highwatermark = main_block ;
                        endorsement_highwatermark = main_endorsement } ;
         test_chain = { block_highwatermark = test_block ;
                        endorsement_highwatermark = test_endorsement } })
    (obj2
       (req "main_chain" highwatermarks_encoding)
       (req "test_chain" highwatermarks_encoding))

let empty_highwatermarks =
  { block_highwatermark = Signature.Public_key_hash.Map.empty ;
    endorsement_highwatermark = Signature.Public_key_hash.Map.empty }

let empty = { main_chain = empty_highwatermarks ;
              test_chain = empty_highwatermarks }

(* We do not lock these functions. The caller will be already locked. *)
let load_watermarks (wallet : #Client_context.wallet) : t tzresult Lwt.t =
  wallet#load name encoding ~default:empty

let save_watermarks (wallet : #Client_context.wallet) highwatermarks : unit tzresult Lwt.t =
  wallet#write name highwatermarks encoding

let retrieve_highwatermark cctxt ~chain =
  load_watermarks cctxt >>=? fun { main_chain ; test_chain } ->
  match chain with
  | `Main -> return main_chain
  | `Test -> return test_chain
  | `Hash chain_id ->
      Chain_services.chain_id cctxt ~chain:`Main () >>=? fun main_chain_id ->
      if Chain_id.equal chain_id main_chain_id then
        return main_chain
      else
        Chain_services.chain_id cctxt ~chain:`Test () >>=? fun test_chain_id ->
        if Chain_id.equal chain_id test_chain_id then
          return test_chain
        else
          fail (Invalid_chain_id chain_id)

let may_inject_block (cctxt : #Client_context.full)  ~chain ~delegate level =
  retrieve_highwatermark cctxt ~chain >>=? fun { block_highwatermark } ->
  match Signature.Public_key_hash.Map.find_opt delegate block_highwatermark with
  | None -> return_true
  | Some past_level -> return Raw_level.(past_level < level)

let may_inject_endorsement (cctxt : #Client_context.wallet) ~chain ~delegate level =
  retrieve_highwatermark cctxt ~chain >>=? fun { endorsement_highwatermark } ->
  match Signature.Public_key_hash.Map.find_opt delegate endorsement_highwatermark with
  | None -> return_true
  | Some past_level -> return Raw_level.(past_level < level)

let record_block (cctxt : #Client_context.wallet) ~chain ~delegate level =
  load_watermarks cctxt >>=? fun ({ main_chain ; test_chain } as watermarks) ->
  let update_main_block_highwatermark () =
    let updated_map =
      Signature.Public_key_hash.Map.update delegate (fun _ -> Some level)
        main_chain.block_highwatermark
    in
    let highwatermarks =
      { watermarks with
        main_chain = { watermarks.main_chain with block_highwatermark = updated_map } } in
    save_watermarks cctxt highwatermarks
  in
  let update_test_block_highwatermark () =
    let updated_map =
      Signature.Public_key_hash.Map.update delegate (fun _ -> Some level)
        test_chain.block_highwatermark
    in
    let highwatermarks =
      { watermarks with
        test_chain = { watermarks.test_chain with block_highwatermark = updated_map } } in
    save_watermarks cctxt highwatermarks
  in
  begin match chain with
    | `Main -> update_main_block_highwatermark ()
    | `Test -> update_test_block_highwatermark ()
    | `Hash chain_id ->
        Chain_services.chain_id cctxt ~chain:`Main () >>=? fun main_chain_id ->
        if Chain_id.equal chain_id main_chain_id then
          update_main_block_highwatermark ()
        else
          Chain_services.chain_id cctxt ~chain:`Test () >>=? fun test_chain_id ->
          if Chain_id.equal chain_id test_chain_id then
            update_test_block_highwatermark ()
          else
            fail (Invalid_chain_id chain_id)
  end

let record_endorsement (cctxt : #Client_context.wallet) ~chain ~delegate level =
  load_watermarks cctxt >>=? fun ({ main_chain ; test_chain } as watermarks) ->
  let update_main_endorsement_highwatermark () =
    let updated_map =
      Signature.Public_key_hash.Map.update delegate (fun _ -> Some level)
        main_chain.endorsement_highwatermark
    in
    let highwatermarks =
      { watermarks with
        main_chain = { watermarks.main_chain with endorsement_highwatermark = updated_map } } in
    save_watermarks cctxt highwatermarks
  in
  let update_test_endorsement_highwatermark () =
    let updated_map =
      Signature.Public_key_hash.Map.update delegate (fun _ -> Some level)
        test_chain.endorsement_highwatermark
    in
    let highwatermarks =
      { watermarks with
        test_chain = { watermarks.test_chain with endorsement_highwatermark = updated_map } } in
    save_watermarks cctxt highwatermarks
  in
  begin match chain with
    | `Main -> update_main_endorsement_highwatermark ()
    | `Test -> update_test_endorsement_highwatermark ()
    | `Hash chain_id ->
        Chain_services.chain_id cctxt ~chain:`Main () >>=? fun main_chain_id ->
        if Chain_id.equal chain_id main_chain_id then
          update_main_endorsement_highwatermark ()
        else
          Chain_services.chain_id cctxt ~chain:`Test () >>=? fun test_chain_id ->
          if Chain_id.equal chain_id test_chain_id then
            update_test_endorsement_highwatermark ()
          else
            fail (Invalid_chain_id chain_id)
  end

let legacy_block_filename = "block"
let legacy_endorsement_filename = "endorsement"
let legacy_encoding = Data_encoding.assoc Raw_level.encoding

let should_upgrade_blocks_file (cctxt : #Client_context.full) =
  cctxt#load legacy_block_filename ~default:[] legacy_encoding >>= function
  | Ok blocks when blocks <> [] -> return_true
  | _ -> return_false (* upgrade not needed or already occured *)

let should_upgrade_endorsements_file (cctxt : #Client_context.full) =
  cctxt#load legacy_endorsement_filename ~default:[] legacy_encoding >>= function
  | Ok endorsements when endorsements <> [] -> return_true
  | _ -> return_false (* upgrade not needed or already occured *)

let upgrade_files (wallet : #Client_context.full) =
  load_watermarks wallet >>=? fun highwatermarks ->
  (* If the file is not empty, it means the upgrade already occured *)
  if highwatermarks <> empty then
    return_unit
  else
    begin
      Client_keys.get_keys wallet >>=? fun keys ->
      return (List.map (fun (_,pkh,_,_) -> pkh) keys)
    end >>=? fun delegates ->
    let delegates_assoc =
      List.map (fun delegate -> Signature.Public_key_hash.to_short_b58check delegate, delegate)
        delegates in
    wallet#load legacy_block_filename ~default:[] legacy_encoding >>=? fun old_blocks ->
    wallet#load legacy_endorsement_filename ~default:[] legacy_encoding >>=? fun old_endorsements ->
    (* Backup the files first *)
    wallet#write
      (legacy_block_filename ^ "_old")
      old_blocks legacy_encoding >>=? fun () ->
    wallet#write
      (legacy_endorsement_filename ^ "_old")
      old_endorsements legacy_encoding >>=? fun () ->

    let highwatermarks =
      List.fold_left (fun ({ main_chain = { block_highwatermark ; _ } } as acc) (delegate, level) ->
          let delegate = List.assoc delegate delegates_assoc in
          match Signature.Public_key_hash.Map.find_opt delegate block_highwatermark with
          | None ->
              let block_highwatermark = Signature.Public_key_hash.Map.add delegate level block_highwatermark in
              { acc with main_chain = { acc.main_chain with block_highwatermark } }
          | Some old_level ->
              if Raw_level.(old_level < level) then
                let block_highwatermark = Signature.Public_key_hash.Map.add delegate level block_highwatermark in
                { acc with main_chain = { acc.main_chain with block_highwatermark } }
              else
                acc
        ) empty old_blocks in
    let highwatermarks =
      List.fold_left (fun ({ main_chain = { endorsement_highwatermark ; _ } } as acc) (delegate, level) ->
          let delegate = List.assoc delegate delegates_assoc in
          match Signature.Public_key_hash.Map.find_opt delegate endorsement_highwatermark with
          | None ->
              let endorsement_highwatermark = Signature.Public_key_hash.Map.add delegate level endorsement_highwatermark in
              { acc with main_chain = { acc.main_chain with endorsement_highwatermark } }
          | Some old_level ->
              if Raw_level.(old_level < level) then
                let endorsement_highwatermark = Signature.Public_key_hash.Map.add delegate level endorsement_highwatermark in
                { acc with main_chain = { acc.main_chain with endorsement_highwatermark } }
              else
                acc
        ) highwatermarks old_endorsements in
    save_watermarks wallet highwatermarks >>=? fun () ->
    (* If given empty, [write] delete the files *)
    wallet#write legacy_block_filename [] legacy_encoding >>=? fun () ->
    wallet#write legacy_endorsement_filename [] legacy_encoding >>=? fun () ->
    return_unit
