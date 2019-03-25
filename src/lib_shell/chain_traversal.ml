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

open State

let path (b1: Block.t) (b2: Block.t) =
  if not (Chain_id.equal (Block.chain_id b1) (Block.chain_id b2)) then
    invalid_arg "Chain_traversal.path" ;
  let rec loop acc current =
    if Block.equal b1 current then
      Lwt.return_some acc
    else
      Block.predecessor current >>= function
      | Some pred -> loop (current :: acc) pred
      | None -> Lwt.return_none in
  loop [] b2

let common_ancestor (b1: Block.t) (b2: Block.t) =
  if not ( Chain_id.equal (Block.chain_id b1) (Block.chain_id b2)) then
    invalid_arg "Chain_traversal.path" ;
  let rec loop (b1: Block.t) (b2: Block.t) =
    if Block.equal b1 b2 then
      Lwt.return b1
    else if Time.Protocol.(Block.timestamp b1 <= Block.timestamp b2) then
      Block.predecessor b2 >>= function
      | None -> assert false
      | Some b2 -> loop b1 b2
    else
      Block.predecessor b1 >>= function
      | None -> assert false
      | Some b1 -> loop b1 b2 in
  loop b1 b2

let iter_predecessors ?max ?min_fitness ?min_date heads ~f =
  let module Local = struct exception Exit end in
  let compare b1 b2 =
    match Fitness.compare (Block.fitness b1) (Block.fitness b2) with
    | 0 -> begin
        match Time.Protocol.compare (Block.timestamp b1) (Block.timestamp b2) with
        | 0 -> Block.compare b1 b2
        | res -> res
      end
    | res -> res in
  let pop, push =
    (* Poor-man priority queue *)
    let queue : Block.t list ref = ref [] in
    let pop () =
      match !queue with
      | [] -> None
      | b :: bs -> queue := bs ; Some b in
    let push b =
      let rec loop = function
        | [] -> [b]
        | b' :: bs' as bs ->
            let cmp = compare b b' in
            if cmp = 0 then
              bs
            else if cmp < 0 then
              b' :: loop bs'
            else
              b :: bs in
      queue := loop !queue in
    pop, push in
  let check_count =
    match max with
    | None -> (fun () -> ())
    | Some max ->
        let cpt = ref 0 in
        fun () ->
          if !cpt >= max then raise Local.Exit ;
          incr cpt in
  let check_fitness =
    match min_fitness with
    | None -> (fun _ -> true)
    | Some min_fitness ->
        (fun b -> Fitness.compare min_fitness (Block.fitness b) <= 0) in
  let check_date =
    match min_date with
    | None -> (fun _ -> true)
    | Some min_date ->
        (fun b -> Time.Protocol.(min_date <= Block.timestamp b)) in
  let rec loop () =
    match pop () with
    | None -> Lwt.return_unit
    | Some b ->
        check_count () ;
        f b >>= fun () ->
        Block.predecessor b >>= function
        | None -> loop ()
        | Some p ->
            if check_fitness p && check_date p then push p ;
            loop () in
  List.iter push heads ;
  try loop () with Local.Exit -> Lwt.return_unit

let iter_predecessors ?max ?min_fitness ?min_date heads ~f =
  match heads with
  | [] -> Lwt.return_unit
  | b :: _ ->
      let chain_id = Block.chain_id b in
      if not (List.for_all (fun b -> Chain_id.equal chain_id (Block.chain_id b)) heads) then
        invalid_arg "State.Helpers.iter_predecessors" ;
      iter_predecessors ?max ?min_fitness ?min_date heads ~f

let new_blocks ~from_block ~to_block =
  common_ancestor from_block to_block >>= fun ancestor ->
  path ancestor to_block >>= function
  | None -> assert false
  | Some path -> Lwt.return (ancestor, path)

let live_blocks block n =
  let rec loop bacc oacc chain_state block_head n =
    Block.Header.all_operation_hashes chain_state block_head >>= fun hashes ->
    let oacc =
      List.fold_left
        (List.fold_left
           (fun oacc op -> Operation_hash.Set.add op oacc))
        oacc hashes  in
    let bacc = Block_hash.Set.add (Block.Header.hash block_head) bacc in
    if n = 0 then Lwt.return (bacc, oacc)
    else
      Block.Header.predecessor chain_state block_head >>= function
      | None -> Lwt.return (bacc, oacc)
      | Some predecessor -> loop bacc oacc chain_state predecessor (pred n) in
  loop
    Block_hash.Set.empty Operation_hash.Set.empty
    (Block.chain_state block) (Block.Header.of_block block)
    n
