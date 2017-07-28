(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad
open Hash
open Store

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let genesis_time =
  Time.of_seconds 0L

let genesis = {
  State.Net.time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

(** *)

let wrap_store_init f base_dir =
  let root = base_dir // "store" in
  Store.init root >>= function
  | Ok store ->
      f store >>= fun () ->
      return ()
  | Error err ->
      Format.kasprintf Pervasives.failwith
        "@[Cannot initialize store:@ %a@]" pp_print_error err

let wrap_raw_store_init f base_dir =
  let root = base_dir // "store" in
  Raw_store.init root >>= function
  | Ok store ->
      f store >>= fun () ->
      return ()
  | Error err ->
      Format.kasprintf Pervasives.failwith
        "@[Cannot initialize store:@ %a@]" pp_print_error err

let test_init _ = Lwt.return_unit

let net_id = Net_id.of_block_hash genesis_block

(** Operation store *)

let make proto : Tezos_data.Operation.t =
  { shell = { net_id ; branch = genesis_block } ; proto }

let op1 = make (MBytes.of_string "Capadoce")
let oph1 = Tezos_data.Operation.hash op1
let op2 = make (MBytes.of_string "Kivu")
let oph2 = Tezos_data.Operation.hash op2


(** Block store *)

let lolblock ?(operations = []) header =
  let operations_hash =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute operations] in
  { Store.Block.header =
      { Block_header.shell =
          { timestamp = Time.of_seconds (Random.int64 1500L) ;
            level = 0l ; (* dummy *)
            proto_level = 0 ; (* dummy *)
            net_id ;
            predecessor = genesis_block ; operations_hash ;
            fitness = [MBytes.of_string @@ string_of_int @@ String.length header;
                       MBytes.of_string @@ string_of_int @@ 12] } ;
        proto = MBytes.of_string header ;
      } ;
    operation_list_count = Random.int 32 ;
    max_operations_ttl = 0 ;
    message = "" ;
  }

let b1 = lolblock "Blop !"
let bh1 = Block_header.hash b1.header
let b2 = lolblock "Tacatlopo"
let bh2 = Block_header.hash b2.header
let b3 = lolblock ~operations:[oph1;oph2] "Persil"
let bh3 = Block_header.hash b3.header
let bh3' =
  let raw = Bytes.of_string @@ Block_hash.to_string bh3 in
  Bytes.set raw 31 '\000' ;
  Bytes.set raw 30 '\000' ;
  Block_hash.of_string_exn @@ Bytes.to_string raw

let equal (b1: Store.Block.contents) (b2: Store.Block.contents) =
  Block_header.equal b1.header b2.header &&
  b1.message = b2.message &&
  b1.operation_list_count = b2.operation_list_count

let check_block s h b =
  Store.Block.Contents.read (s, h) >>= function
  | Ok b' when equal b b' -> Lwt.return_unit
  | Ok _ ->
      Printf.eprintf "Error while reading block %s\n%!" (Block_hash.to_hex h);
      exit 1
  | Error err ->
      Format.eprintf "@[Error while reading block %s:@ %a\n@]"
        (Block_hash.to_hex h)
        pp_print_error err;
      exit 1

let test_block s =
  let s = Store.Net.get s net_id in
  let s = Store.Block.get s in
  Block.Contents.store (s, bh1) b1 >>= fun () ->
  Block.Contents.store (s, bh2) b2 >>= fun () ->
  Block.Contents.store (s, bh3) b3 >>= fun () ->
  check_block s bh1 b1 >>= fun () ->
  check_block s bh2 b2 >>= fun () ->
  check_block s bh3 b3

let test_expand s =
  let s = Store.Net.get s net_id in
  let s = Store.Block.get s in
  Block.Contents.store (s, bh1) b1 >>= fun () ->
  Block.Contents.store (s, bh2) b2 >>= fun () ->
  Block.Contents.store (s, bh3) b3 >>= fun () ->
  Block.Contents.store (s, bh3') b3 >>= fun () ->
  Base58.complete (Block_hash.to_short_b58check bh1) >>= fun res ->
  Assert.equal_string_list ~msg:__LOC__ res [Block_hash.to_b58check bh1] ;
  Base58.complete (Block_hash.to_short_b58check bh2) >>= fun res ->
  Assert.equal_string_list ~msg:__LOC__ res [Block_hash.to_b58check bh2] ;
  Base58.complete (Block_hash.to_short_b58check bh3) >>= fun res ->
  Assert.equal_string_list ~msg:__LOC__
    (List.sort String.compare res)
    [Block_hash.to_b58check bh3' ; Block_hash.to_b58check bh3] ;
  Lwt.return_unit


(** Generic store *)

let check (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) k d =
  Store.read_opt s k >|= fun d' ->
  if d' <> Some d then begin
    Assert.fail_msg
      "Error while reading key %S\n%!"  (String.concat Filename.dir_sep k) ;
  end

let check_none (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) k =
  Store.read_opt s k >|= function
  | None -> ()
  | Some _ ->
      Assert.fail_msg
        "Error while reading non-existent key %S\n%!"
        (String.concat Filename.dir_sep k)

let test_generic (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) =
  Store.store s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
  Store.store s ["day";"next"] (MBytes.of_string "Jeudi") >>= fun () ->
  Store.store s ["day";"truc";"chose"] (MBytes.of_string "Vendredi") >>= fun () ->
  check (module Store) s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
  check (module Store) s ["day";"next"] (MBytes.of_string "Jeudi") >>= fun () ->
  check_none (module Store) s ["day"]

let list (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) k =
  Store.fold_keys s k ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

let test_generic_list (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) =
  Store.store s ["a"; "b"] (MBytes.of_string "Novembre") >>= fun () ->
  Store.store s ["a"; "c"] (MBytes.of_string "Juin") >>= fun () ->
  Store.store s ["a"; "d"; "e"] (MBytes.of_string "Septembre") >>= fun () ->
  Store.store s ["f";] (MBytes.of_string "Avril") >>= fun () ->
  Store.store s ["g"; "h"] (MBytes.of_string "Avril") >>= fun () ->
  list (module Store) s [] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__
    [["a";"b"];["a";"c"];["a";"d";"e"];["f"];["g";"h"]]
    (List.sort compare l) ;
  list (module Store) s ["a"] >>= fun l ->
  Assert.equal_string_list_list
    ~msg:__LOC__ [["a";"b"]; ["a";"c"]; ["a";"d";"e"]]
    (List.sort compare l) ;
  list (module Store) s ["f"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  list (module Store) s ["g"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [["g";"h"]] (List.sort compare l) ;
  list (module Store) s ["i"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  Lwt.return_unit

(** HashSet *)

open Store_helpers

let test_hashset (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) =
  let module BlockSet = Block_hash.Set in
  let module StoreSet =
    Make_buffered_set
      (Make_substore(Store)(struct let name = ["test_set"] end))
      (Block_hash)
      (BlockSet) in
  let open BlockSet in
  let bhset : BlockSet.t = BlockSet.add bh2 (BlockSet.add bh1 BlockSet.empty) in
  StoreSet.store_all s bhset >>= fun () ->
  StoreSet.read_all s >>= fun bhset' ->
  Assert.equal_block_set ~msg:__LOC__ bhset bhset' ;
  let bhset2 =
    Pervasives.(bhset |> BlockSet.add bh3 |> BlockSet.remove bh1) in
  StoreSet.store_all s bhset2 >>= fun () ->
  StoreSet.read_all s >>= fun bhset2' ->
  Assert.equal_block_set ~msg:__LOC__ bhset2 bhset2' ;
  StoreSet.fold s BlockSet.empty
    (fun bh acc -> Lwt.return (BlockSet.add bh acc)) >>= fun bhset2'' ->
  Assert.equal_block_set ~msg:__LOC__ bhset2 bhset2'' ;
  Store.store s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
  StoreSet.remove_all s >>= fun () ->
  StoreSet.read_all s >>= fun empty ->
  Assert.equal_block_set ~msg:__LOC__ BlockSet.empty empty ;
  check (module Store) s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
  Lwt.return_unit


(** HashMap *)

let test_hashmap (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) =
  let module BlockMap = Block_hash.Map in
  let module StoreMap =
    Make_buffered_map
      (Make_substore(Store)(struct let name = ["test_map"] end))
      (Block_hash)
      (Make_value(struct
         type t = int * char
         let encoding =
           Data_encoding.(tup2 int31 (conv int_of_char char_of_int int8))
       end))
      (BlockMap) in
  let eq = (=) in
  let map =
    Pervasives.(BlockMap.empty |>
                BlockMap.add bh1 (1, 'a') |> BlockMap.add bh2 (2, 'b')) in
  StoreMap.store_all s map >>= fun () ->
  StoreMap.read_all s >>= fun map' ->
  Assert.equal_block_map ~msg:__LOC__ ~eq map map' ;
  let map2 =
    Pervasives.(map |> BlockMap.add bh3 (3, 'c') |> BlockMap.remove bh1) in
  StoreMap.store_all s map2 >>= fun () ->
  StoreMap.read_all s >>= fun map2' ->
  Assert.equal_block_map ~msg:__LOC__ ~eq map2 map2' ;
  Lwt.return_unit

(** Functors *)

let test_single (type t)
    (module Store: Store_sigs.STORE with type t = t) (s: Store.t) =
  let module Single =
    Make_single_store
      (Store)
      (struct let name = ["plop"] end)
      (Make_value(struct
         type t = int * string
         let encoding = Data_encoding.(tup2 int31 string)
       end)) in
  Single.known s >>= fun known ->
  Assert.is_false ~msg:__LOC__ known ;
  Single.read_opt s >>= fun v' ->
  Assert.equal ~msg:__LOC__ None v' ;
  let v = (3, "Non!") in
  Single.store s v >>= fun () ->
  Single.known s >>= fun known ->
  Assert.is_true ~msg:__LOC__ known ;
  Single.read_opt s >>= fun v' ->
  Assert.equal ~msg:__LOC__ (Some v) v' ;
  Single.remove s >>= fun () ->
  Single.known s >>= fun known ->
  Assert.is_false ~msg:__LOC__ known ;
  Single.read_opt s >>= fun v' ->
  Assert.equal ~msg:__LOC__ None v' ;
  Lwt.return_unit

module Sub =
  Make_substore(Raw_store)(struct let name = ["plop";"plip"] end)

module SubBlocks =
  Make_indexed_substore
    (Make_substore(Raw_store)(struct let name = ["blocks"] end))
    (Block_hash)

module SubBlocksSet =
  SubBlocks.Make_buffered_set
    (struct let name = ["test_set"] end)
    (Block_hash.Set)

module SubBlocksMap =
  SubBlocks.Make_buffered_map
    (struct let name = ["test_map"] end)
    (Make_value(struct
       type t = int * string
       let encoding = Data_encoding.(tup2 int31 string)
     end))
    (Block_hash.Map)

let test_subblock s =
  SubBlocksSet.known s bh1 >>= fun known ->
  Assert.is_false ~msg:__LOC__ known ;
  SubBlocksSet.store s bh1 >>= fun () ->
  SubBlocksSet.store s bh2 >>= fun () ->
  SubBlocksSet.known s bh2 >>= fun known ->
  Assert.is_true ~msg:__LOC__ known ;
  SubBlocksSet.read_all s >>= fun set ->
  let set' =
    Block_hash.Set.empty
    |> Block_hash.Set.add bh1
    |> Block_hash.Set.add bh2 in
  Assert.equal_block_set ~msg:__LOC__ set set' ;
  SubBlocksSet.remove s bh2 >>= fun () ->
  let set =
    Block_hash.Set.empty
    |> Block_hash.Set.add bh3'
    |> Block_hash.Set.add bh3 in
  SubBlocksSet.store_all s set >>= fun () ->
  SubBlocksSet.elements s >>= fun elts ->
  Assert.equal_block_hash_list ~msg:__LOC__
    (List.sort Block_hash.compare elts)
    (List.sort Block_hash.compare [bh3 ; bh3']) ;
  SubBlocksSet.store s bh2 >>= fun () ->
  SubBlocksSet.remove s bh3 >>= fun () ->
  SubBlocksSet.elements s >>= fun elts ->
  Assert.equal_block_hash_list ~msg:__LOC__
    (List.sort Block_hash.compare elts)
    (List.sort Block_hash.compare [bh2 ; bh3']) ;
  SubBlocksMap.known s bh1 >>= fun known ->
  Assert.is_false ~msg:__LOC__ known ;
  let v1 = (3, "Non!")
  and v2 = (12, "Beurk.") in
  SubBlocksMap.store s bh1 v1 >>= fun () ->
  SubBlocksMap.store s bh2 v2 >>= fun () ->
  SubBlocksMap.known s bh1 >>= fun known ->
  SubBlocksMap.read_opt s bh1 >>= fun v1' ->
  Assert.equal ~msg:__LOC__ (Some v1) v1' ;
  Assert.is_true ~msg:__LOC__ known ;
  let map =
    Block_hash.Map.empty
    |> Block_hash.Map.add bh1 v1
    |> Block_hash.Map.add bh2 v2 in
  SubBlocksMap.read_all s >>= fun map' ->
  Assert.equal_block_map ~eq:(=) ~msg:__LOC__ map map' ;

  SubBlocksSet.remove_all s >>= fun () ->
  SubBlocksSet.elements s >>= fun elts ->
  Assert.equal_block_hash_list ~msg:__LOC__ elts [] ;

  SubBlocksMap.read_all s >>= fun map' ->
  Assert.equal_block_map ~eq:(=) ~msg:__LOC__ map map' ;

  SubBlocksSet.store s bh3 >>= fun () ->

  SubBlocks.indexes s >>= fun keys ->
  Assert.equal_block_hash_list ~msg:__LOC__
    (List.sort Block_hash.compare keys)
    (List.sort Block_hash.compare [bh1;bh2;bh3]) ;

  Lwt.return_unit

module SubSubBlocks =
  Make_indexed_substore
    (Make_substore(SubBlocks.Store)(struct let name = ["sub_blocks"] end))
    (Block_hash)

(** *)

let tests_raw : (string * (Raw_store.t -> unit Lwt.t)) list = [

  "init", test_init ;

  "generic", test_generic (module Raw_store) ;
  "generic_substore", test_generic (module Sub) ;
  "generic_indexedstore",
  (fun s -> test_generic (module SubBlocks.Store) (s, bh1)) ;
  "generic_indexedsubstore",
  (fun s -> test_generic (module SubSubBlocks.Store) ((s, bh1), bh2)) ;

  "single", test_single (module Raw_store) ;
  "single_substore", test_single (module Sub) ;
  "single_indexedstore",
  (fun s -> test_single (module SubBlocks.Store) (s, bh1)) ;
  "single_indexedsubstore",
  (fun s -> test_single (module SubSubBlocks.Store) ((s, bh1), bh2)) ;

  "generic_list", test_generic_list (module Raw_store);
  "generic_substore_list", test_generic_list (module Sub);
  "generic_indexedstore_list",
  (fun s -> test_generic_list (module SubBlocks.Store) (s, bh1));
  "generic_indexedsubstore_list",
  (fun s -> test_generic_list (module SubSubBlocks.Store) ((s, bh1), bh2)) ;

  "hashset", test_hashset (module Raw_store) ;
  "hashset_substore", test_hashset (module Sub) ;
  "hashset_indexedstore",
  (fun s -> test_hashset (module SubBlocks.Store) (s, bh1));
  "hashset_indexedsubstore",
  (fun s -> test_hashset (module SubSubBlocks.Store) ((s, bh1), bh2)) ;

  "hashmap", test_hashmap (module Raw_store) ;
  "hashmap_substore", test_hashmap (module Sub) ;
  "hashmap_indexedstore",
  (fun s -> test_hashmap (module SubBlocks.Store) (s, bh1));
  "hashmap_indexedsubstore",
  (fun s -> test_hashmap (module SubSubBlocks.Store) ((s, bh1), bh2)) ;

  "subblock", test_subblock ;

]

let tests : (string * (Store.t -> unit Lwt.t)) list = [
  "expand", test_expand ;
  "block", test_block ;
]

let () =
  Test.run "store."
    (List.map (fun (s, f) -> s, wrap_raw_store_init f) tests_raw @
     List.map (fun (s, f) -> s, wrap_store_init f) tests)
