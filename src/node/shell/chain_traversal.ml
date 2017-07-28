(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open State

let path (b1: Block.t) (b2: Block.t) =
  if not (Net_id.equal (Block.net_id b1) (Block.net_id b2)) then
    invalid_arg "Chain_traversal.path" ;
  let rec loop acc current =
    if Block.equal b1 current then
      Lwt.return (Some acc)
    else
      Block.predecessor current >>= function
      | Some pred -> loop (current :: acc) pred
      | None -> Lwt.return_none in
  loop [] b2

let common_ancestor (b1: Block.t) (b2: Block.t) =
  if not ( Net_id.equal (Block.net_id b1) (Block.net_id b2)) then
    invalid_arg "Chain_traversal.path" ;
  let rec loop (b1: Block.t) (b2: Block.t) =
    if Block.equal b1 b2 then
      Lwt.return b1
    else if Time.(Block.timestamp b1 <= Block.timestamp b2) then
      Block.predecessor b2 >>= function
      | None -> assert false
      | Some b2 -> loop b1 b2
    else
      Block.predecessor b1 >>= function
      | None -> assert false
      | Some b1 -> loop b1 b2 in
  loop b1 b2

let block_locator (b: Block.t) sz =
  let rec loop acc sz step cpt b =
    if sz = 0 then
      Lwt.return (List.rev acc)
    else
      Block.predecessor b >>= function
      | None ->
          Lwt.return (List.rev (Block.hash b :: acc))
      | Some predecessor ->
          if cpt = 0 then
            loop (Block.hash b :: acc) (sz - 1)
                 (step * 2) (step * 20 - 1) predecessor
          else if cpt mod step = 0 then
            loop (Block.hash b :: acc) (sz - 1)
                 step (cpt - 1) predecessor
          else
            loop acc sz step (cpt - 1) predecessor in
  loop [] sz 1 9 b

let iter_predecessors ?max ?min_fitness ?min_date heads ~f =
  let module Local = struct exception Exit end in
  let compare b1 b2 =
    match Fitness.compare (Block.fitness b1) (Block.fitness b2) with
    | 0 -> begin
        match Time.compare (Block.timestamp b1) (Block.timestamp b2) with
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
        (fun b -> Time.(min_date <= Block.timestamp b)) in
  let rec loop () =
    match pop () with
    | None -> Lwt.return ()
    | Some b ->
        check_count () ;
        f b >>= fun () ->
        Block.predecessor b >>= function
        | None -> loop ()
        | Some p ->
            if check_fitness p && check_date p then push p ;
            loop () in
  List.iter push heads ;
  try loop () with Local.Exit -> Lwt.return ()

let iter_predecessors ?max ?min_fitness ?min_date heads ~f =
  match heads with
  | [] -> Lwt.return_unit
  | b :: _ ->
      let net_id = Block.net_id b in
      if not (List.for_all (fun b -> Net_id.equal net_id (Block.net_id b)) heads) then
        invalid_arg "State.Helpers.iter_predecessors" ;
      iter_predecessors ?max ?min_fitness ?min_date heads ~f

let new_blocks ~from_block ~to_block =
  common_ancestor from_block to_block >>= fun ancestor ->
  path ancestor to_block >>= function
  | None -> assert false
  | Some path -> Lwt.return (ancestor, path)

let live_blocks block n =
  let rec loop bacc oacc block n =
    Block.all_operation_hashes block >>= fun hashes ->
    let oacc =
      List.fold_left
        (List.fold_left
           (fun oacc op -> Operation_hash.Set.add op oacc))
        oacc hashes  in
    let bacc = Block_hash.Set.add (Block.hash block) bacc in
    if n = 0 then Lwt.return (bacc, oacc)
    else
      Block.predecessor block >>= function
      | None -> Lwt.return (bacc, oacc)
      | Some predecessor -> loop bacc oacc predecessor (pred n) in
  loop Block_hash.Set.empty Operation_hash.Set.empty block n
