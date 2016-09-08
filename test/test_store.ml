
open Utils
open Hash
open Store

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b48check
    "Et22nEeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

let genesis_protocol =
  Protocol_hash.of_b48check
    "JF7Fxgeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

let genesis_time =
  Time.of_seconds 0L

let genesis = {
  Store.time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

(** *)

let wrap_store_init f base_dir =
  let root = base_dir // "store" in
  Store.init root >>= fun store ->
  f store

let test_init _ = Lwt.return_unit

(** Operation store *)

let make proto : Store.operation =
  { shell = { net_id = Net genesis_block } ; proto }

let op1 = make (MBytes.of_string "Capadoce")
let oph1 = Operation.hash op1
let op2 = make (MBytes.of_string "Kivu")
let oph2 = Operation.hash op2

let check_operation s h b =
  Operation.get s h >>= function
  | Some { Time.data = Ok b' } when Operation.equal b b' -> Lwt.return_unit
  | _ ->
      Printf.eprintf "Error while reading operation %s\n%!"
        (Operation_hash.to_hex h);
      exit 1

let test_operation s =
  Persist.use s.operation (fun s ->
      Operation.set s oph1 (Time.make_timed (Ok op1)) >>= fun () ->
      Operation.set s oph2 (Time.make_timed (Ok op2)) >>= fun () ->
      check_operation s oph1 op1 >>= fun () ->
      check_operation s oph2 op2)

(** Block store *)

let lolblock ?(operations = []) header =
  { Time.time = Time.of_seconds (Random.int64 1500L) ;
    data =
      { shell =
          { timestamp = Time.of_seconds (Random.int64 1500L) ;
            net_id = Store.Net genesis_block ;
            predecessor = genesis_block ; operations;
            fitness = [MBytes.of_string @@ string_of_int @@ String.length header;
                       MBytes.of_string @@ string_of_int @@ 12] } ;
        proto = MBytes.of_string header ;
      } ;
  }

let b1 = lolblock "Blop !"
let bh1 = Store.Block.hash b1.data
let b2 = lolblock "Tacatlopo"
let bh2 = Store.Block.hash b2.data
let b3 = lolblock ~operations:[oph1;oph2] "Persil"
let bh3 = Store.Block.hash b3.data

let check_block s h b =
  Block.full_get s h >>= function
  | Some b' when Store.Block.equal b.Time.data b'.Time.data
              && Time.equal b.time b'.time -> Lwt.return_unit
  | Some b' ->
      Printf.eprintf "Error while reading block %s\n%!" (Block_hash.to_hex h);
      exit 1
  | None ->
      Printf.eprintf "Error while reading block %s (not found)\n%!"
        (Block_hash.to_hex h);
      exit 1

let test_block (s: Store.store) =
  Persist.use s.block (fun s ->
      Block.full_set s bh1 b1 >>= fun () ->
      Block.full_set s bh2 b2 >>= fun () ->
      Block.full_set s bh3 b3 >>= fun () ->
      check_block s bh1 b1 >>= fun () ->
      check_block s bh2 b2 >>= fun () ->
      check_block s bh3 b3)


(** Generic store *)

let check s k d =
  get s k >|= fun d' ->
  if d' <> Some d then begin
    Test.fail
      "Error while reading key %S\n%!"
      (String.concat Filename.dir_sep k);
  end

let check_none s k =
  get s k >|= function
  | None -> ()
  | Some _ ->
      Test.fail
        "Error while reading non-existent key %S\n%!"
        (String.concat Filename.dir_sep k)

let test_generic (s: Store.store) =
  Persist.use s.global_store (fun s ->
      set s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
      set s ["day";"next"] (MBytes.of_string "Jeudi") >>= fun () ->
      set s ["day";"truc";"chose"] (MBytes.of_string "Vendredi") >>= fun () ->
      check s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
      check s ["day";"next"] (MBytes.of_string "Jeudi") >>= fun () ->
      check_none s ["day"])

let test_generic_list (s: Store.store) =
  Persist.use s.global_store (fun s ->
      set s ["a"; "b"] (MBytes.of_string "Novembre") >>= fun () ->
      set s ["a"; "c"] (MBytes.of_string "Juin") >>= fun () ->
      set s ["a"; "d"; "e"] (MBytes.of_string "Septembre") >>= fun () ->
      set s ["f";] (MBytes.of_string "Avril") >>= fun () ->
      set s ["g"; "h"] (MBytes.of_string "Avril") >>= fun () ->
      list s [] >>= fun l ->
      assert (l = []);
      list s [[]] >>= fun l ->
      assert (l = [["a"];["f"];["g"];["version"]]);
      list s [["a"]] >>= fun l ->
      assert (l = [["a";"b"]; ["a";"c"]; ["a";"d"]]);
      list s [["f"]] >>= fun l ->
      assert (l = []);
      list s [["g"]] >>= fun l ->
      assert (l = [["g";"h"]]);
      list s [["i"]] >>= fun l ->
      assert (l = []);
      list s [["a"];["g"]] >>= fun l ->
      assert (l = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["g"; "h"]]);
      Lwt.return_unit)

(** HashSet *)

let test_hashset (s: Store.store) =
  let test name b =
    if b then Lwt.return_unit else Test.fail name in
  let module BlockSet = Hash_set(Block_hash) in
  let module StoreSet =
    Persist.MakeBufferedPersistentSet
      (Store.Faked_functional_store)
      (struct
        include Block_hash
        let prefix = [ "test_set" ]
        let length = path_len
      end)(BlockSet) in
  let bhset = BlockSet.empty |> BlockSet.add bh1 |> BlockSet.add bh2 in
  Persist.use s.global_store (fun s ->
      StoreSet.write s bhset >>= fun s ->
      StoreSet.read s >>= fun bhset' ->
      test "init" (BlockSet.compare bhset bhset' = 0) >>= fun () ->
      let bhset2 = bhset |> BlockSet.add bh3 |> BlockSet.remove bh1 in
      StoreSet.write s bhset2 >>= fun s ->
      StoreSet.read s >>= fun bhset2' ->
      test "add/del" (BlockSet.compare bhset2 bhset2' = 0) >>= fun () ->
      StoreSet.fold s BlockSet.empty
        (fun bh acc -> Lwt.return (BlockSet.add bh acc)) >>= fun bhset2'' ->
      test "fold" (BlockSet.compare bhset2 bhset2'' = 0) >>= fun () ->
      set s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
      StoreSet.clear s >>= fun s ->
      StoreSet.read s >>= fun empty ->
      test "clean" (BlockSet.compare empty BlockSet.empty = 0) >>= fun () ->
      check s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
      Lwt.return_unit)


(** HashMap *)

let test_hashmap (s: Store.store) =
  let test name b =
    if b then Lwt.return_unit else Test.fail name in
  let module BlockMap = Hash_map(Block_hash) in
  let module StoreMap =
    Persist.MakeBufferedPersistentTypedMap
      (Store.Faked_functional_store)
      (struct
        include Block_hash
        let prefix = [ "test_map" ]
        let length = path_len
      end)
      (struct
        type value = int * char
        let encoding =
          Data_encoding.(tup2 int31 (conv int_of_char char_of_int int8))
      end)
      (BlockMap) in
  let map =
    BlockMap.empty |> BlockMap.add bh1 (1, 'a') |> BlockMap.add bh2 (2, 'b') in
  Persist.use s.global_store (fun s ->
      StoreMap.write s map >>= fun s ->
      StoreMap.read s >>= fun map' ->
      test "init" (BlockMap.compare Pervasives.compare map map' = 0) >>= fun () ->
      let map2 = map |> BlockMap.add bh3 (3, 'c') |> BlockMap.remove bh1 in
      StoreMap.write s map2 >>= fun s ->
      StoreMap.read s >>= fun map2' ->
      test "add/del"
        (BlockMap.compare Pervasives.compare map2 map2' = 0) >>= fun () ->
      Lwt.return_unit)

(** *)

let tests : (string * (store -> unit Lwt.t)) list = [
  "init", test_init ;
  "operation", test_operation ;
  "block", test_block ;
  "generic", test_generic ;
  "generic_list", test_generic_list ;
  "hashset", test_hashset ;
  "hashmap", test_hashmap ;
 ]

let res =
  Test.run "store." (List.map (fun (s, f) -> s, wrap_store_init f) tests)

