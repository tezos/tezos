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

let encoding =
  let open Data_encoding in
  def "highwatermarks" @@
  assoc Raw_level.encoding

let empty = []

(* We do not lock these functions. The caller will be already locked. *)
let load_highwatermarks (cctxt : #Proto_alpha.full) filename : t tzresult Lwt.t =
  cctxt#load filename encoding ~default:empty

let save_highwatermarks (cctxt : #Proto_alpha.full) filename highwatermarks : unit tzresult Lwt.t =
  cctxt#write filename highwatermarks encoding

let retrieve_highwatermark cctxt filename =
  load_highwatermarks cctxt filename

let may_inject (cctxt : #Proto_alpha.full) location ~delegate level =
  retrieve_highwatermark cctxt (Client_baking_files.filename location) >>=? fun highwatermark ->
  let delegate = Signature.Public_key_hash.to_short_b58check delegate in
  List.find_opt
    (fun (delegate', _) -> String.compare delegate delegate' = 0)
    highwatermark |> function
  | None -> return_true
  | Some (_, past_level) -> return Raw_level.(past_level < level)

let may_inject_block = may_inject
let may_inject_endorsement = may_inject

let record (cctxt : #Proto_alpha.full) location ~delegate level =
  let filename = Client_baking_files.filename location in
  let delegate = Signature.Public_key_hash.to_short_b58check delegate in
  load_highwatermarks cctxt filename >>=? fun highwatermarks ->
  let level = match List.assoc_opt delegate highwatermarks with
    | None -> level
    | Some lower_prev_level when level >= lower_prev_level -> level
    | Some higher_prev_level -> higher_prev_level (* should only happen in `forced` mode *) in
  save_highwatermarks cctxt filename
    ((delegate, level) ::
     (List.filter (fun (delegate', _) ->
          String.compare delegate delegate' <> 0
        ) highwatermarks))

let record_block = record
let record_endorsement = record
