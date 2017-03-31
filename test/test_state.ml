(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash
open Error_monad

let (//) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let genesis_time =
  Time.of_seconds 0L

module Proto = (val Updater.get_exn genesis_protocol)

let genesis : State.Net.genesis = {
  time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

let net_id = Net_id.of_block_hash genesis_block

let incr_fitness fitness =
  let new_fitness =
    match fitness with
    | [ _ ; fitness ] ->
        Pervasives.(
          Data_encoding.Binary.of_bytes Data_encoding.int64 fitness
          |> Utils.unopt ~default:0L
          |> Int64.succ
          |> Data_encoding.Binary.to_bytes Data_encoding.int64
        )
    | _ -> Data_encoding.Binary.to_bytes Data_encoding.int64 1L
  in
  [ MBytes.of_string "\000" ; new_fitness ]

let incr_timestamp timestamp =
  Time.add timestamp (Int64.add 1L (Random.int64 10L))

let operation op =
  let op : Store.Operation.t = {
    shell = { net_id } ;
    proto = MBytes.of_string op ;
  } in
  Store.Operation.hash op,
  op,
  Data_encoding.Binary.to_bytes Store.Operation.encoding op

let block state ?(operations = []) pred_hash pred name : Store.Block_header.t =
  let operations =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute operations] in
  let fitness = incr_fitness pred.Store.Block_header.shell.fitness in
  let timestamp = incr_timestamp pred.shell.timestamp in
  { shell = {
        net_id = pred.shell.net_id ;
        predecessor = pred_hash ;
        timestamp ; operations; fitness } ;
    proto = MBytes.of_string name ;
  }

let build_chain state tbl otbl pred names =
  Lwt_list.fold_left_s
    (fun (pred_hash, pred) name ->
       begin
         let oph, op, bytes = operation name in
         State.Operation.store state oph op >>= fun created ->
         Assert.is_true ~msg:__LOC__ created ;
         State.Operation.read_opt state oph >>= fun op' ->
         Assert.equal_operation ~msg:__LOC__ (Some op) op' ;
         State.Operation.mark_invalid state oph [] >>= fun store_invalid ->
         Assert.is_true ~msg:__LOC__ store_invalid ;
         Hashtbl.add otbl name (oph, Error []) ;
         let block = block ~operations:[oph] state pred_hash pred name in
         let hash = Store.Block_header.hash block in
         State.Block_header.store state hash block >>= fun created ->
         Assert.is_true ~msg:__LOC__ created ;
         State.Block_header.read_opt state hash >>= fun block' ->
         Assert.equal_block ~msg:__LOC__ (Some block) block' ;
         State.Block_header.mark_invalid state hash [] >>= fun store_invalid ->
         Assert.is_true ~msg:__LOC__ store_invalid ;
         Hashtbl.add tbl name (hash, block) ;
         return (hash, block)
       end >>= function
       | Ok v -> Lwt.return v
       | Error err ->
           Error_monad.pp_print_error Format.err_formatter err ;
           assert false)
    pred
    names >>= fun _ ->
  Lwt.return ()

let block state ?(operations = []) (pred: State.Valid_block.t) name
  : State.Block_header.t =
  let operations =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute operations] in
  let fitness = incr_fitness pred.fitness in
  let timestamp = incr_timestamp pred.timestamp in
  { shell = { net_id = pred.net_id ;
              predecessor = pred.hash ;
              timestamp ; operations; fitness } ;
    proto = MBytes.of_string name ;
  }

let build_valid_chain state tbl vtbl otbl pred names =
  Lwt_list.fold_left_s
    (fun pred name ->
       begin
         let oph, op, bytes = operation name in
         State.Operation.store state oph op >>= fun created ->
         Assert.is_true ~msg:__LOC__ created ;
         State.Operation.read_opt state oph >>= fun op' ->
         Assert.equal_operation ~msg:__LOC__ (Some op) op' ;
         Hashtbl.add otbl name (oph, Ok op) ;
         let block = block state ~operations:[oph] pred name in
         let hash = Store.Block_header.hash block in
         State.Block_header.store state hash block >>= fun created ->
         Assert.is_true ~msg:__LOC__ created ;
         State.Operation_list.store_all state hash [[oph]] >>= fun () ->
         State.Block_header.read_opt state hash >>= fun block' ->
         Assert.equal_block ~msg:__LOC__ (Some block) block' ;
         Hashtbl.add tbl name (hash, block) ;
         Lwt.return (Proto.parse_block block pred.timestamp) >>=? fun block ->
         Proto.apply pred.context block [] >>=? fun ctxt ->
         State.Valid_block.store state hash ctxt >>=? fun _vblock ->
         State.Valid_block.read state hash >>=? fun vblock ->
         Hashtbl.add vtbl name vblock ;
         return vblock
       end >>= function
       | Ok v -> Lwt.return v
       | Error err ->
           Error_monad.pp_print_error Format.err_formatter err ;
           assert false)
    pred
    names >>= fun _ ->
  Lwt.return ()

let build_example_tree net =
  let tbl = Hashtbl.create 23 in
  let vtbl = Hashtbl.create 23 in
  let otbl = Hashtbl.create 23 in
  State.Valid_block.Current.genesis net >>= fun genesis ->
  State.Block_header.read_exn net genesis.hash >>= fun genesis_header ->
  Hashtbl.add vtbl "Genesis" genesis ;
  Hashtbl.add tbl "Genesis" (genesis.hash, genesis_header ) ;
  let chain = [ "A1" ; "A2" ; "A3" ; "A4" ; "A5" ; "A6" ; "A7" ; "A8" ] in
  build_valid_chain net tbl vtbl otbl genesis chain >>= fun () ->
  let a3 = Hashtbl.find vtbl "A3" in
  let chain = [ "B1" ; "B2" ; "B3" ; "B4" ; "B5" ; "B6" ; "B7" ; "B8" ] in
  build_valid_chain net tbl vtbl otbl a3 chain >>= fun () ->
  let b7 = Hashtbl.find tbl "B7" in
  let chain = [ "C1" ; "C2" ; "C3" ; "C4" ; "C5" ; "C6" ; "C7" ; "C8" ] in
  build_chain net tbl otbl b7 chain >>= fun () ->
  let pending_op = "PP" in
  let oph, op, bytes = operation pending_op in
  State.Operation.store net oph op >>= fun _ ->
  State.Operation.read_opt net oph >>= fun op' ->
  Assert.equal_operation ~msg:__LOC__ (Some op) op' ;
  Hashtbl.add otbl pending_op (oph, Ok op) ;
  Lwt.return (tbl, vtbl, otbl)

type state = {
  block: (string, Block_hash.t * Store.Block_header.t) Hashtbl.t ;
  operation: (string, Operation_hash.t * Store.Operation.t tzresult) Hashtbl.t ;
  vblock: (string, State.Valid_block.t) Hashtbl.t ;
  state: State.t ;
  net: State.Net.t ;
  init: unit -> State.t tzresult Lwt.t;
}

let block s = Hashtbl.find s.block
let vblock s = Hashtbl.find s.vblock
let operation s = Hashtbl.find s.operation

exception Found of string
let rev_find s h =
  try
    Hashtbl.iter (fun k (bh,_) ->
        if Block_hash.equal bh h then raise (Found k))
      s.block ;
    Format.asprintf "genesis(%a)" Block_hash.pp_short h
  with Found s -> s

let blocks s =
  Hashtbl.fold (fun k v acc -> (k,v) :: acc) s.block []
  |> List.sort Pervasives.compare

let vblocks s =
  Hashtbl.fold (fun k v acc -> (k,v) :: acc) s.vblock []
  |> List.sort Pervasives.compare

let operations s =
  Hashtbl.fold (fun k v acc -> (k,v) :: acc) s.operation []
  |> List.sort Pervasives.compare

let wrap_state_init f base_dir =
  begin
    let store_root = base_dir // "store" in
    let context_root = base_dir // "context" in
    let init () =
      State.read
        ~store_root
        ~context_root
        () in
    init () >>=? fun state ->
    State.Net.create state genesis >>= fun net ->
    build_example_tree net >>= fun (block, vblock, operation) ->
    f { state ; net ; block ; vblock ; operation ; init } >>=? fun s ->
    return ()
  end

let test_init (s: state) =
  return ()

let test_read_operation (s: state) =
  Lwt_list.iter_s (fun (name, (oph, op)) ->
      State.Operation.invalid s.net oph >>= function
      | Some err ->
          begin match op with
            | Ok _ ->
                Assert.fail_msg "Incorrect invalid operation read %s" name
            | Error e ->
                if e <> err then
                  Assert.fail_msg "Incorrect operation read %s" name ;
                Lwt.return_unit
          end
      | None ->
          State.Operation.read_opt s.net oph >>= function
          | None ->
              Assert.fail_msg "Cannot read block %s" name
          | Some data ->
              begin match op with
                | Error _ ->
                    Assert.fail_msg "Incorrect valid operation read %s" name
                | Ok op ->
                    if op.Store.Operation.proto <> data.proto then
                      Assert.fail_msg "Incorrect operation read %s %s" name
                        (MBytes.to_string data.Store.Operation.proto) ;
                    Lwt.return_unit
              end)
    (operations s) >>= fun () ->
  return ()



(****************************************************************************)

(** State. *)

let test_read_block (s: state) =
  Lwt_list.iter_s (fun (name, (hash, block)) ->
      begin
        State.Block_header.read_opt s.net hash >>= function
        | None ->
            Assert.fail_msg "Cannot read block %s" name
        | Some block' ->
            if not (Store.Block_header.equal block block') then
              Assert.fail_msg "Error while reading block %s" name ;
            Lwt.return_unit
      end >>= fun () ->
      let vblock =
        try Some (vblock s name)
        with Not_found -> None in
      State.Valid_block.read s.net hash >>= function
      | Error _ ->
          if vblock <> None then
            Assert.fail_msg "Error while reading valid block %s" name ;
          Lwt.return_unit
      | Ok _vblock' ->
          match vblock with
          | None ->
              Assert.fail_msg "Error while reading invalid block %s" name
          | Some _vblock ->
              Lwt.return_unit
    ) (blocks s) >>= fun () ->
  return ()


(****************************************************************************)

(** State.successors *)

let compare s kind name succs l =
  if Block_hash.Set.cardinal succs <> List.length l then
    Assert.fail_msg
      "unexpected %ssuccessors size (%s: %d %d)"
      kind name (Block_hash.Set.cardinal succs) (List.length l) ;
  List.iter
    (fun bname ->
       let bh = fst @@ block s bname in
       if not (Block_hash.Set.mem bh succs) then
         Assert.fail_msg
           "missing block in %ssuccessors (%s: %s)" kind name bname)
    l

let test_successors s =
  let test s name expected invalid_expected =
    let b = vblock s name in
    State.Valid_block.read s.net b.hash >>= function
    | Error _ ->
        Assert.fail_msg "Failed while reading block %s" name
    | Ok { successors ; invalid_successors } ->
        compare s "" name successors expected ;
        compare s "invalid " name invalid_successors invalid_expected ;
        Lwt.return_unit

  in
  test s "A1" ["A2"] [] >>= fun () ->
  test s "A3" ["A4";"B1"] [] >>= fun () ->
  test s "A8" [] [] >>= fun () ->
  test s "B1" ["B2"] [] >>= fun () ->
  test s "B7" ["B8"] ["C1"] >>= fun () ->
  return ()


(****************************************************************************)

(** State.path *)

let rec compare_path p1 p2 = match p1, p2 with
  | [], [] -> true
  | h1 :: p1, h2 :: p2 -> Block_hash.equal h1 h2 && compare_path p1 p2
  | _ -> false

let test_path (s: state) =
  let check_path h1 h2 p2 =
    State.Block_header.Helpers.path s.net (fst @@ block s h1) (fst @@ block s h2) >>= function
    | Error _ ->
        Assert.fail_msg "cannot compute path %s -> %s" h1 h2
    | Ok p1 ->
        let p1 = List.map (fun b -> fst b) p1 in
        let p2 = List.map (fun b -> fst (block s b)) p2 in
        if not (compare_path p1 p2) then
          Assert.fail_msg "bad path %s -> %s" h1 h2 ;
        Lwt.return_unit in
  check_path "Genesis" "Genesis" [] >>= fun () ->
  check_path "A1" "A1" [] >>= fun () ->
  check_path "A2" "A6" ["A3"; "A4"; "A5"; "A6"] >>= fun () ->
  check_path "B2" "B6" ["B3"; "B4"; "B5"; "B6"] >>= fun () ->
  check_path "A1" "B3" ["A2"; "A3"; "B1"; "B2"; "B3"] >>= fun () ->
  check_path "A1" "C2" ["A2"; "A3"; "B1"; "B2"; "B3" ; "B4" ;
                        "B5" ; "B6" ; "B7" ; "C1" ; "C2" ] >>= fun () ->
  return ()

let test_valid_path (s: state) =
  let check_path h1 h2 p2 =
    State.Valid_block.Helpers.path s.net (vblock s h1) (vblock s h2) >>= function
    | None ->
        Assert.fail_msg "cannot compute path %s -> %s" h1 h2 ;
    | Some (p: State.Valid_block.t list) ->
        let p = List.map (fun b -> b.State.Valid_block.hash) p in
        let p2 = List.map (fun b -> (vblock s b).hash) p2 in
        if not (compare_path p p2) then
          Assert.fail_msg "bad path %s -> %s" h1 h2 ;
        Lwt.return_unit in
  check_path "Genesis" "Genesis" [] >>= fun () ->
  check_path "A1" "A1" [] >>= fun () ->
  check_path "A2" "A6" ["A3"; "A4"; "A5"; "A6"] >>= fun () ->
  check_path "B2" "B6" ["B3"; "B4"; "B5"; "B6"] >>= fun () ->
  check_path "A1" "B3" ["A2"; "A3"; "B1"; "B2"; "B3"] >>= fun () ->
  return ()


(****************************************************************************)

(** State.ancestor *)

let test_ancestor s =
  let check_ancestor h1 h2 expected =
    State.Block_header.Helpers.common_ancestor
      s.net (fst @@ block s h1) (fst @@ block s h2) >>= function
    | Error _ ->
        Assert.fail_msg "Cannot compure ancestor for %s %s" h1 h2 ;
    | Ok (a, _) ->
        if not (Block_hash.equal a (fst expected)) then
          Assert.fail_msg
            "bad ancestor %s %s: found %s, expected %s"
            h1 h2 (rev_find s a) (rev_find s @@ fst expected) ;
        Lwt.return_unit in
  let check_valid_ancestor h1 h2 expected =
    State.Valid_block.Helpers.common_ancestor
      s.net (vblock s h1) (vblock s h2) >>= fun a ->
    if not (Block_hash.equal a.hash expected.State.Valid_block.hash) then
      Assert.fail_msg "bad ancestor %s %s" h1 h2 ;
    Lwt.return_unit in
  check_ancestor "Genesis" "Genesis" (block s "Genesis") >>= fun () ->
  check_ancestor "Genesis" "A3" (block s "Genesis") >>= fun () ->
  check_ancestor "A3" "Genesis" (block s "Genesis") >>= fun () ->
  check_ancestor "A1" "A1" (block s "A1") >>= fun () ->
  check_ancestor "A1" "A3" (block s "A1") >>= fun () ->
  check_ancestor "A3" "A1" (block s "A1") >>= fun () ->
  check_ancestor "A6" "B6" (block s "A3") >>= fun () ->
  check_ancestor "B6" "A6" (block s "A3") >>= fun () ->
  check_ancestor "A4" "B1" (block s "A3") >>= fun () ->
  check_ancestor "B1" "A4" (block s "A3") >>= fun () ->
  check_ancestor "A3" "B1" (block s "A3") >>= fun () ->
  check_ancestor "B1" "A3" (block s "A3") >>= fun () ->
  check_ancestor "A2" "B1" (block s "A2") >>= fun () ->
  check_ancestor "B1" "A2" (block s "A2") >>= fun () ->
  check_ancestor "C4" "B8" (block s "B7") >>= fun () ->
  check_ancestor "B8" "C4" (block s "B7") >>= fun () ->
  check_ancestor "C4" "A8" (block s "A3") >>= fun () ->
  check_ancestor "A8" "C4" (block s "A3") >>= fun () ->
  check_valid_ancestor "A6" "B6" (vblock s "A3") >>= fun () ->
  check_valid_ancestor "B6" "A6" (vblock s "A3") >>= fun () ->
  check_valid_ancestor "A4" "B1" (vblock s "A3") >>= fun () ->
  check_valid_ancestor "B1" "A4" (vblock s "A3") >>= fun () ->
  check_valid_ancestor "A3" "B1" (vblock s "A3") >>= fun () ->
  check_valid_ancestor "B1" "A3" (vblock s "A3") >>= fun () ->
  check_valid_ancestor "A2" "B1" (vblock s "A2") >>= fun () ->
  check_valid_ancestor "B1" "A2" (vblock s "A2") >>= fun () ->
  return ()


(****************************************************************************)

(** State.locator *)

let test_locator s =
  let check_locator h1 expected =
    State.Block_header.Helpers.block_locator
      s.net (List.length expected) (fst @@ block s h1) >>= function
    | Error _ ->
        Assert.fail_msg "Cannot compute locator for %s" h1
    | Ok l ->
        if List.length l <> List.length expected then
          Assert.fail_msg
            "Invalid locator length %s (found: %d, expected: %d)"
            h1 (List.length l) (List.length expected) ;
        List.iter2
          (fun h h2 ->
             if not (Block_hash.equal h (fst @@ block s h2)) then
               Assert.fail_msg "Invalid locator %s (expectd: %s)" h1 h2)
          l expected;
        Lwt.return_unit in
  let check_valid_locator h1 expected =
    State.Valid_block.Helpers.block_locator
      s.net (List.length expected) (vblock s h1) >>= fun l ->
    if List.length l <> List.length expected then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h1 (List.length l) (List.length expected) ;
    List.iter2
      (fun h h2 ->
         if not (Block_hash.equal h (fst @@ block s h2)) then
           Assert.fail_msg "Invalid locator %s (expectd: %s)" h1 h2)
      l expected ;
    Lwt.return_unit in
  check_locator "A8" ["A8";"A7";"A6";"A5";"A4";"A3";"A2";"A1"] >>= fun () ->
  check_locator "B8"
    ["B8";"B7";"B6";"B5";"B4";"B3";"B2";"B1";"A3"] >>= fun () ->
  check_locator "C8"
    ["C8";"C7";"C6";"C5";"C4";"C3";"C2";"C1";
     "B7";"B6";"B4";"B2";"A3";"A1"] >>= fun () ->
  check_locator "C8" ["C8";"C7";"C6";"C5";"C4"] >>= fun () ->
  check_valid_locator "A8"
    ["A8";"A7";"A6";"A5";"A4";"A3";"A2"] >>= fun () ->
  check_valid_locator "B8"
    ["B8";"B7";"B6";"B5";"B4";"B3";"B2";"B1";"A3"] >>= fun () ->
  check_valid_locator "B8" ["B8";"B7";"B6";"B5";"B4"] >>= fun () ->
  return ()


(****************************************************************************)

(** State.known_heads *)

let compare s name heads l =
  if List.length heads <> List.length l then
    Assert.fail_msg
      "unexpected known_heads size (%s: %d %d)"
      name (List.length heads) (List.length l) ;
  List.iter
    (fun bname ->
       let hash = (vblock s bname).hash in
       if not (List.exists (fun b -> Block_hash.equal hash b.State.Valid_block.hash) heads) then
         Assert.fail_msg "missing block in known_heads (%s: %s)" name bname)
    l

let test_known_heads s =
  State.Valid_block.known_heads s.net >>= fun heads ->
  compare s "initial" heads ["A8";"B8"] ;
  return ()


(****************************************************************************)

(** State.head/set_head *)

let test_head s =
  State.Valid_block.Current.head s.net >>= fun head ->
  if not (Block_hash.equal head.hash genesis_block) then
    Assert.fail_msg "unexpected head" ;
  State.Valid_block.Current.set_head s.net (vblock s "A6") >>= fun _ ->
  State.Valid_block.Current.head s.net >>= fun head ->
  if not (Block_hash.equal head.hash (vblock s "A6").hash) then
    Assert.fail_msg "unexpected head" ;
  return ()


(****************************************************************************)

(** State.mem *)

let test_mem s =
  let mem s x =
    State.Valid_block.Current.mem s.net (fst @@ block s x) in
  let test_mem s x =
    mem s x >>= function
    | true -> Lwt.return_unit
    | false -> Assert.fail_msg "mem %s" x in
  let test_not_mem s x =
    mem s x >>= function
    | false -> Lwt.return_unit
    | true -> Assert.fail_msg "not (mem %s)" x in
  test_not_mem s "A3" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  State.Valid_block.Current.set_head s.net (vblock s "A8") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_mem s "A6" >>= fun () ->
  test_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  State.Valid_block.Current.set_head s.net (vblock s "A6") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  State.Valid_block.Current.set_head s.net (vblock s "B6") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_not_mem s "A4" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_mem s "B1" >>= fun () ->
  test_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  State.Valid_block.Current.set_head s.net (vblock s "B8") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_not_mem s "A4" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_mem s "B1" >>= fun () ->
  test_mem s "B6" >>= fun () ->
  test_mem s "B8" >>= fun () ->
  return ()


(****************************************************************************)

(** State.new *)

let test_new s =
  let test s h expected =
    State.Valid_block.Helpers.block_locator s.net 50 (vblock s h) >>= fun loc ->
    State.Valid_block.Current.find_new s.net loc (List.length expected) >>= function
    | Error _ ->
        Assert.fail_msg "Failed to compute new blocks %s" h
    | Ok blocks ->
        if List.length blocks <> List.length expected then
          Assert.fail_msg
            "Invalid locator length %s (found: %d, expected: %d)"
            h (List.length blocks) (List.length expected) ;
        List.iter2
          (fun h1 h2 ->
             if not (Block_hash.equal h1 (vblock s h2).hash) then
               Assert.fail_msg "Invalid locator %s (expected: %s)" h h2)
          blocks expected ;
        Lwt.return_unit
  in
  test s "A6" [] >>= fun () ->
  State.Valid_block.Current.set_head s.net (vblock s "A8") >>= fun _ ->
  test s "A6" ["A7";"A8"] >>= fun () ->
  test s "A6" ["A7"] >>= fun () ->
  test s "B4" ["A4"] >>= fun () ->
  test s "B7" ["A4";"A5";"A6";"A7"] >>= fun () ->
  return ()


(****************************************************************************)

(** State.mempool *)

let compare s name mempool l =
  let mempool_sz = Operation_hash.Set.cardinal mempool in
  let l_sz = List.length l in
  if mempool_sz <> l_sz then
    Assert.fail
      (string_of_int mempool_sz)
      (string_of_int l_sz)
      "unexpected mempool size (%s)" name ;
  List.iter
    (fun oname ->
       try
         let oph = fst @@ operation s oname  in
         if not (Operation_hash.Set.mem oph mempool) then
           Assert.fail_msg "missing operation in mempool (%s: %s)" name oname
       with Not_found ->
         Assert.fail_msg "Read value not found in mempool (%s: %s)" name oname)
    l

let test_mempool s =
  State.Operation.list_pending s.net >>= fun mempool ->
  compare s "initial" mempool
    ["PP";
     "A1" ; "A2" ; "A3" ; "A4" ; "A5" ; "A6" ; "A7" ; "A8" ;
     "B1" ; "B2" ; "B3" ; "B4" ; "B5" ; "B6" ; "B7" ; "B8" ] ;
  State.Valid_block.Current.set_head s.net (vblock s "A8") >>= fun _ ->
  State.Operation.list_pending s.net >>= fun mempool ->
  compare s "A8" mempool
    ["PP"; "B1" ; "B2" ; "B3" ; "B4" ; "B5" ; "B6" ; "B7" ; "B8" ] ;
  State.Valid_block.Current.set_head s.net (vblock s "A6") >>= fun _ ->
  State.Operation.list_pending s.net >>= fun mempool ->
  compare s "A6" mempool
    ["PP";
     "A7" ; "A8" ;
     "B1" ; "B2" ; "B3" ; "B4" ; "B5" ; "B6" ; "B7" ; "B8" ] ;
  State.Valid_block.Current.set_head s.net (vblock s "B6") >>= fun _ ->
  State.Operation.list_pending s.net >>= fun mempool ->
  compare s "B6" mempool
    ["PP";
     "A4" ; "A5" ; "A6" ; "A7" ; "A8" ;
     "B7" ; "B8" ] ;
  State.Operation.mark_invalid s.net (fst @@ operation s "PP") [] >>= fun rm_status ->
  Assert.is_true ~msg:__LOC__ rm_status ;
  State.Operation.mark_invalid s.net (fst @@ operation s "PP") [] >>= fun rm_status ->
  Assert.is_false ~msg:__LOC__ rm_status ;
  State.Operation.list_pending s.net >>= fun mempool ->
  compare s "B6.remove" mempool
    ["A4" ; "A5" ; "A6" ; "A7" ; "A8" ;
     "B7" ; "B8" ] ;
  return ()

(****************************************************************************)


let tests : (string * (state -> unit tzresult Lwt.t)) list = [
  "init", test_init ;
  "read_operation", test_read_operation;
  "read_block", test_read_block ;
  "successors", test_successors ;
  "path", test_path ;
  "valid_path", test_valid_path ;
  "ancestor", test_ancestor ;
  "locator", test_locator ;
  "known_heads", test_known_heads ;
  "head", test_head ;
  "mem", test_mem ;
  "new", test_new ;
  "mempool", test_mempool;
]

let () =
  Test.run "state." (List.map (fun (s, f) -> s, wrap_state_init f) tests)
