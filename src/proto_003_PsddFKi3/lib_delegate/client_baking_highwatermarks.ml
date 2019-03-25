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

type error += Level_previously_endorsed of Raw_level.t
type error += Level_previously_baked of Raw_level.t

let () = begin
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"highwatermarks.block_already_baked"
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
    ~id:"highwatermarks.block_already_endorsed"
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

type t = (string * Raw_level.t) list

let endorsements_basename = "endorsement"
let blocks_basename = "block"

let encoding =
  let open Data_encoding in
  def "highwatermarks" @@
  assoc Raw_level.encoding

let empty = []

let resolve_filename (cctxt : #Proto_alpha.full) ~chain basename =
  let test_filename chain_id =
    Format.kasprintf return "test_%a_%s" Chain_id.pp_short chain_id basename in
  match chain with
  | `Main -> return basename
  | `Test ->
      Chain_services.chain_id cctxt ~chain:`Test () >>=? fun chain_id ->
      test_filename chain_id
  | `Hash chain_id ->
      Chain_services.chain_id cctxt ~chain:`Main () >>=? fun main_chain_id ->
      if Chain_id.(chain_id = main_chain_id) then
        return basename
      else
        test_filename chain_id

let resolve_blocks_filename cctxt ~chain =
  resolve_filename cctxt ~chain blocks_basename

let resolve_endorsements_filename cctxt ~chain =
  resolve_filename cctxt ~chain endorsements_basename

(* We do not lock these functions. The caller will be already locked. *)
let load_highwatermarks (cctxt : #Proto_alpha.full) filename : t tzresult Lwt.t =
  cctxt#load filename encoding ~default:empty

let save_highwatermarks (cctxt : #Proto_alpha.full) filename highwatermarks : unit tzresult Lwt.t =
  cctxt#write filename highwatermarks encoding

let retrieve_highwatermark cctxt filename =
  load_highwatermarks cctxt filename

let may_inject_block (cctxt : #Proto_alpha.full) ~chain ~delegate level =
  resolve_blocks_filename cctxt ~chain >>=? fun blocks_filename ->
  retrieve_highwatermark cctxt blocks_filename  >>=? fun blocks_highwatermark ->
  let delegate = Signature.Public_key_hash.to_short_b58check delegate in
  List.find_opt
    (fun (delegate', _) -> String.compare delegate delegate' = 0)
    blocks_highwatermark |> function
  | None -> return_true
  | Some (_, past_level) -> return Raw_level.(past_level < level)

let may_inject_endorsement (cctxt : #Proto_alpha.full) ~chain ~delegate level =
  resolve_endorsements_filename cctxt ~chain >>=? fun endorsements_filename ->
  retrieve_highwatermark cctxt endorsements_filename  >>=? fun endorsements_highwatermark ->
  let delegate = Signature.Public_key_hash.to_short_b58check delegate in
  List.find_opt
    (fun (delegate', _) -> String.compare delegate delegate' = 0)
    endorsements_highwatermark |> function
  | None -> return_true
  | Some (_, past_level) -> return Raw_level.(past_level < level)

let record_block (cctxt : #Proto_alpha.full) ~chain ~delegate level =
  let delegate = Signature.Public_key_hash.to_short_b58check delegate in
  resolve_blocks_filename cctxt ~chain >>=? fun blocks_filename ->
  load_highwatermarks cctxt blocks_filename >>=? fun highwatermarks ->
  save_highwatermarks cctxt blocks_filename
    ((delegate, level) ::
     (List.filter (fun (delegate', _) ->
          String.compare delegate delegate' <> 0
        ) highwatermarks))

let record_endorsement (cctxt : #Proto_alpha.full) ~chain ~delegate level =
  let delegate = Signature.Public_key_hash.to_short_b58check delegate in
  resolve_endorsements_filename cctxt ~chain >>=? fun endorsements_filename ->
  load_highwatermarks cctxt endorsements_filename >>=? fun highwatermarks ->
  save_highwatermarks cctxt endorsements_filename
    ((delegate, level) ::
     (List.filter (fun (delegate', _) ->
          String.compare delegate delegate' <> 0
        ) highwatermarks))
