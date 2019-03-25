(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Chain_services

let get_chain_id state = function
  | `Main -> Lwt.return (State.Chain.main state)
  | `Test -> begin
      State.Chain.get_exn state (State.Chain.main state) >>= fun main_chain ->
      State.Chain.test main_chain >>= function
      | None -> Lwt.fail Not_found
      | Some chain_id -> Lwt.return chain_id
    end
  | `Hash chain_id ->
      Lwt.return chain_id

let get_chain_id_opt state chain =
  Lwt.catch
    (fun () -> get_chain_id state chain >>= Lwt.return_some)
    (fun _exn -> Lwt.return_none)

let get_chain state chain =
  get_chain_id state chain >>= fun chain_id ->
  State.Chain.get_exn state chain_id

let predecessors ignored length head =
  let rec loop acc length block =
    if length <= 0 then
      Lwt.return (List.rev acc)
    else
      State.Block.predecessor block >>= function
      | None ->
          Lwt.return (List.rev acc)
      | Some pred ->
          if Block_hash.Set.mem (State.Block.hash block) ignored then
            Lwt.return (List.rev acc)
          else
            loop (State.Block.hash pred :: acc) (length-1) pred
  in
  loop [State.Block.hash head] (length-1) head

let list_blocks chain_state ?(length = 1) ?min_date heads =
  begin
    match heads with
    | [] ->
        Chain.known_heads chain_state >>= fun heads ->
        let heads =
          match min_date with
          | None -> heads
          | Some min_date ->
              List.fold_left
                (fun acc block ->
                   let timestamp = State.Block.timestamp block in
                   if Time.Protocol.(min_date <= timestamp) then block :: acc
                   else acc)
                [] heads in
        let sorted_heads =
          List.sort
            (fun b1 b2 ->
               let f1 = State.Block.fitness b1 in
               let f2 = State.Block.fitness b2 in
               ~- (Fitness.compare f1 f2))
            heads in
        Lwt.return (List.map (fun b -> Some b) sorted_heads)
    | _ :: _ as heads ->
        Lwt_list.map_p (State.Block.read_opt chain_state) heads
  end >>= fun requested_heads ->
  Lwt_list.fold_left_s
    (fun (ignored, acc) head ->
       match head with
       | None -> Lwt.return (ignored, [])
       | Some block ->
           predecessors ignored length block >>= fun predecessors ->
           let ignored = List.fold_left (fun acc v -> Block_hash.Set.add v acc)
               ignored predecessors in
           Lwt.return (ignored, predecessors :: acc))
    (Block_hash.Set.empty, [])
    requested_heads >>= fun (_, blocks) ->
  return (List.rev blocks)

let rpc_directory =

  let dir : State.Chain.t RPC_directory.t ref =
    ref RPC_directory.empty in

  let register0 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst0 s)
        (fun chain p q -> f chain p q) in
  let register1 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst1 s)
        (fun (chain, a) p q -> f chain a p q) in

  let register_dynamic_directory2 ?descr s f =
    dir :=
      RPC_directory.register_dynamic_directory
        !dir ?descr (RPC_path.subst1 s)
        (fun (chain, a) -> f chain a) in

  register0 S.chain_id begin fun chain () () ->
    return (State.Chain.id chain)
  end ;

  (* blocks *)

  register0 S.Blocks.list begin fun chain q () ->
    list_blocks chain ?length:q#length ?min_date:q#min_date q#heads
  end ;

  register_dynamic_directory2
    Block_services.path
    Block_directory.build_rpc_directory ;

  (* invalid_blocks *)

  register0 S.Invalid_blocks.list begin fun chain () () ->
    let convert (hash, level, errors) = { hash ; level ; errors } in
    State.Block.list_invalid chain >>= fun blocks ->
    return (List.map convert blocks)
  end ;

  register1 S.Invalid_blocks.get begin fun chain hash () () ->
    State.Block.read_invalid chain hash >>= function
    | None -> Lwt.fail Not_found
    | Some { level ; errors } -> return { hash ; level ; errors }
  end ;

  register1 S.Invalid_blocks.delete begin fun chain hash () () ->
    State.Block.unmark_invalid chain hash
  end ;

  !dir

let build_rpc_directory validator =

  let distributed_db = Validator.distributed_db validator in
  let state = Distributed_db.state distributed_db in

  let dir = ref rpc_directory in

  (* Mempool *)

  let merge d = dir := RPC_directory.merge !dir d in
  merge
    (RPC_directory.map
       (fun chain ->
          match Validator.get validator (State.Chain.id chain) with
          | Error _ -> Lwt.fail Not_found
          | Ok chain_validator ->
              Lwt.return (Chain_validator.prevalidator chain_validator))
       Prevalidator.rpc_directory) ;

  RPC_directory.prefix Chain_services.path @@
  RPC_directory.map (fun ((), chain) -> get_chain state chain) !dir
