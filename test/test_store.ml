
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
    Assert.fail_msg
      "Error while reading key %S\n%!"  (String.concat Filename.dir_sep k) ;
  end

let check_none s k =
  get s k >|= function
  | None -> ()
  | Some _ ->
      Assert.fail_msg
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
      Assert.equal_persist_list ~msg:__LOC__ [] l ;
      list s [[]] >>= fun l ->
      Assert.equal_persist_list
        ~msg:__LOC__ [["a"];["f"];["g"];["version"]] l ;
      list s [["a"]] >>= fun l ->
      Assert.equal_persist_list
        ~msg:__LOC__ [["a";"b"]; ["a";"c"]; ["a";"d"]] l ;
      list s [["f"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__ [] l ;
      list s [["g"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__ [["g";"h"]] l ;
      list s [["i"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__ [] l ;
      list s [["a"];["g"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__
        [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["g"; "h"]] l ;
      Lwt.return_unit)

(** HashSet *)

let test_hashset (s: Store.store) =
  let module BlockSet = Hash_set(Block_hash) in
  let module StoreSet =
    Persist.MakeBufferedPersistentSet
      (Store.Faked_functional_store)
      (struct
        include Block_hash
        let prefix = [ "test_set" ]
        let length = path_len
      end)(BlockSet) in
  let open BlockSet in
  let eq = BlockSet.equal in
  let bhset : BlockSet.t = BlockSet.add bh2 (BlockSet.add bh1 BlockSet.empty) in
  Persist.use s.global_store (fun s ->
      StoreSet.write s bhset >>= fun s ->
      StoreSet.read s >>= fun bhset' ->
      Assert.equal_block_map ~msg:__LOC__ ~eq bhset bhset' ;
      let bhset2 =
        Pervasives.(bhset |> BlockSet.add bh3 |> BlockSet.remove bh1) in
      StoreSet.write s bhset2 >>= fun s ->
      StoreSet.read s >>= fun bhset2' ->
      Assert.equal_block_map ~msg:__LOC__ ~eq bhset2 bhset2' ;
      StoreSet.fold s BlockSet.empty
        (fun bh acc -> Lwt.return (BlockSet.add bh acc)) >>= fun bhset2'' ->
      Assert.equal_block_map ~msg:__LOC__ ~eq bhset2 bhset2'' ;
      set s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
      StoreSet.clear s >>= fun s ->
      StoreSet.read s >>= fun empty ->
      Assert.equal_block_map ~msg:__LOC__ ~eq BlockSet.empty empty ;
      check s ["day";"current"] (MBytes.of_string "Mercredi") >>= fun () ->
      Lwt.return_unit)


(** HashMap *)

let test_hashmap (s: Store.store) =
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
  let eq = BlockMap.equal (=) in
  let map =
    Pervasives.(BlockMap.empty |>
                BlockMap.add bh1 (1, 'a') |> BlockMap.add bh2 (2, 'b')) in
  Persist.use s.global_store (fun s ->
      StoreMap.write s map >>= fun s ->
      StoreMap.read s >>= fun map' ->
      Assert.equal_block_map ~msg:__LOC__ ~eq map map' ;
      let map2 =
        Pervasives.(map |> BlockMap.add bh3 (3, 'c') |> BlockMap.remove bh1) in
      StoreMap.write s map2 >>= fun s ->
      StoreMap.read s >>= fun map2' ->
      Assert.equal_block_map ~msg:__LOC__ ~eq map2 map2' ;
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

let () =
  Test.run "store." (List.map (fun (s, f) -> s, wrap_store_init f) tests)
