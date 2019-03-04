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

include Internal_event.Legacy_logging.Make_semantic(struct let name = "client.nonces" end)

type t = Nonce.t Block_hash.Map.t

let empty = Block_hash.Map.empty

let encoding =
  let open Data_encoding in
  def "seed_nonce" @@
  conv
    (fun m ->
       Block_hash.Map.fold (fun hash nonce acc -> (hash, nonce) :: acc) m [])
    (fun l ->
       List.fold_left
         (fun map (hash, nonce) -> Block_hash.Map.add hash nonce map)
         Block_hash.Map.empty l) @@
  list
    (obj2
       (req "block" Block_hash.encoding)
       (req "nonce" Nonce.encoding))

let load (wallet : #Client_context.wallet) location =
  wallet#load (Client_baking_files.filename location) ~default:empty encoding

let save (wallet : #Client_context.wallet) location nonces =
  wallet#write (Client_baking_files.filename location) nonces encoding

let mem nonces hash =
  Block_hash.Map.mem hash nonces

let find_opt nonces hash =
  Block_hash.Map.find_opt hash nonces

let add nonces hash nonce =
  Block_hash.Map.add hash nonce nonces

let add_all nonces nonces_to_add =
  Block_hash.Map.fold (fun hash nonce acc ->
      add acc hash nonce
    ) nonces_to_add nonces

let remove nonces hash =
  Block_hash.Map.remove hash nonces

let remove_all nonces nonces_to_remove =
  Block_hash.Map.fold (fun hash _ acc ->
      remove acc hash
    ) nonces_to_remove nonces

let get_block_level_opt cctxt ~chain ~block =
  Shell_services.Blocks.Header.shell_header cctxt ~chain ~block () >>= function
  | Ok { level ; _ } -> Lwt.return_some level
  | Error errs ->
      lwt_warn Tag.DSL.(fun f ->
          f "@[<v 2>Cannot retrieve block %a header associated to \
             nonce:@ @[%a@]@]@."
          -% t event "cannot_retrieve_block_header"
          -% a Logging.block_tag block
          -% a errs_tag errs) >>= fun () ->
      Lwt.return_none

let get_outdated_nonces cctxt ?constants ~chain nonces =
  begin match constants with
    | None -> Alpha_services.Constants.all cctxt (chain, `Head 0)
    | Some constants -> return constants
  end >>=? fun { Constants.parametric = { blocks_per_cycle ; _ } ; _ } ->
  get_block_level_opt cctxt ~chain ~block:(`Head 0) >>= function
  | None ->
      lwt_log_error Tag.DSL.(fun f ->
          f "Cannot fetch chain's head level. Aborting nonces filtering."
          -% t event "cannot_retrieve_head_level") >>= fun () ->
      return (empty, empty)
  | Some current_level ->
      let current_cycle = Int32.(div current_level blocks_per_cycle) in
      let is_older_than_5_cycles block_level =
        let block_cycle = Int32.(div block_level blocks_per_cycle) in
        Int32.sub current_cycle block_cycle > 5l in
      Block_hash.Map.fold (fun hash nonce acc ->
          acc >>=? fun (orphans, outdated) ->
          get_block_level_opt cctxt ~chain ~block:(`Hash (hash, 0)) >>= function
          | Some level ->
              if is_older_than_5_cycles level then
                return (orphans, (add outdated hash nonce))
              else
                acc
          | None -> return ((add orphans hash nonce), outdated)
        ) nonces (return (empty, empty))

let filter_outdated_nonces cctxt ?constants location nonces =
  let chain = Client_baking_files.chain location in
  get_outdated_nonces cctxt ?constants ~chain nonces >>=? fun (orphans, outdated_nonces) ->
  begin if Block_hash.Map.cardinal orphans >= 50 then
      lwt_warn Tag.DSL.(fun f ->
          f "Found too many nonces associated to blocks unknown by the \
             node in '$TEZOS_CLIENT/%s'. After checking that these \
             blocks were never included in the chain (e.g. via a block \
             explorer), consider using `tezos-client filter orphan \
             nonces` to clear them."
          -% s Logging.filename_tag (Client_baking_files.filename location ^ "s")
          -% t event "too_many_orphans") >>= fun () ->
      Lwt.return_unit
    else
      Lwt.return_unit
  end >>= fun () ->
  return (remove_all nonces outdated_nonces)

let get_unrevealed_nonces cctxt location nonces =
  let chain = Client_baking_files.chain location in
  Client_baking_blocks.blocks_from_current_cycle cctxt
    ~chain (`Head 0)
    ~offset:(-1l) () >>=? fun blocks ->
  filter_map_s (fun hash ->
      match find_opt nonces hash with
      | None -> return_none
      | Some nonce ->
          begin get_block_level_opt cctxt ~chain ~block:(`Hash (hash, 0)) >>= function
            | Some level -> begin
                Lwt.return
                  (Alpha_environment.wrap_error (Raw_level.of_int32 level)) >>=? fun level ->
                Alpha_services.Nonce.get cctxt (chain, `Head 0) level >>=? function
                | Missing nonce_hash
                  when Nonce.check_hash nonce nonce_hash ->
                    lwt_log_notice Tag.DSL.(fun f ->
                        f "Found nonce to reveal for %a (level: %a)"
                        -% t event "found_nonce"
                        -% a Block_hash.Logging.tag hash
                        -% a Logging.level_tag level) >>= fun () ->
                    return_some (level, nonce)
                | Missing _nonce_hash ->
                    lwt_log_error Tag.DSL.(fun f ->
                        f "Incoherent nonce for level %a"
                        -% t event "bad_nonce"
                        -% a Logging.level_tag level)
                    >>= fun () -> return_none
                | Forgotten -> return_none
                | Revealed _ -> return_none end
            | None -> return_none
          end)
    blocks
