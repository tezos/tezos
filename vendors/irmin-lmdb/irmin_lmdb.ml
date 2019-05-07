(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Option = struct
  let of_result = function
    | Ok v -> Some v
    | _ -> None

  let value_map ~default ~f = function
    | None -> default
    | Some v -> f v
end

module Result = struct
  let map ~f = function
    | Ok v -> Ok (f v)
    | Error err -> Error err
end

let cstruct_of_ba_copy ba =
  let cs = Cstruct.of_bigarray ba in
  let len = Cstruct.len cs in
  let cs_copy = Cstruct.create_unsafe len in
  Cstruct.blit cs 0 cs_copy 0 len ;
  cs_copy

open Lwt.Infix

type t = {
  db: Lmdb.t ;
  root: string ;
  mutable wtxn: (Lmdb.rw Lmdb.txn * Lmdb.db) option;
}

let of_result = function
  | Ok v -> Lwt.return v
  | Error err -> Lwt.fail_with (Lmdb.string_of_error err)

let (|>>) v f =
  match v with
  | Ok v -> f v
  | Error e -> Error e

let get_wtxn db =
  match db.wtxn with
  | Some t -> Ok t
  | None ->
      Lmdb.create_rw_txn db.db |>> fun txn ->
      Lmdb.opendb txn |>> fun ddb ->
      db.wtxn <- Some (txn, ddb);
      Ok (txn, ddb)

let commit_wtxn db =
  match db.wtxn with
  | None -> Ok ()
  | Some (t, _ddb) ->
      db.wtxn <- None;
      Lmdb.commit_txn t

let add db k v =
  get_wtxn db |>> fun (txn, ddb) ->
  Lmdb.put_string txn ddb k v

let add db k v =
  of_result @@ add db k v

let add_cstruct db k v =
  get_wtxn db |>> fun (txn, ddb) ->
  Lmdb.put txn ddb k (Cstruct.to_bigarray v)

let add_cstruct db k v =
  of_result @@ add_cstruct db k v

let src = Logs.Src.create "irmin.lmdb" ~doc:"Irmin in a Lmdb store"
module Log = (val Logs.src_log src : Logs.LOG)

let int64_of_string s =
  try Ok (Int64.of_string s)
  with Failure _ ->
    Error (`Msg (Printf.sprintf "%s is not the representation of an int64" s))

let bool_of_string s =
  try Ok (bool_of_string s)
  with Failure _ ->
    Error (`Msg (Printf.sprintf "%s is not the representation of a boolean" s))

let int64_converter = int64_of_string, Fmt.uint64
let bool_converter = bool_of_string, Fmt.bool

module Conf = struct

  let root = Irmin.Private.Conf.root
  let mapsize =
    Irmin.Private.Conf.key "mapsize" int64_converter 409_600_000_000L
  let readonly = 
    Irmin.Private.Conf.key "readonly" bool_converter false 

end

let config
    ?(config=Irmin.Private.Conf.empty) ?mapsize ?(readonly=false) file =
  let module C = Irmin.Private.Conf in
  let config = C.add config Conf.root (Some file) in
  let config = C.add config Conf.readonly readonly in
  Option.value_map mapsize ~default:config ~f:(C.add config Conf.mapsize)

type ('r) reader = { f : 'k. 'k Lmdb.txn -> Lmdb.db -> ('r, Lmdb.error) result } [@@unboxed]

let with_read_db db ~f =
  match db.wtxn with
  | None ->
      Lmdb.with_ro_db db.db ~f:f.f
  | Some (txn, ddb) ->
      f.f txn ddb

let mem db k =
  with_read_db db ~f:{ f = fun txn db -> Lmdb.mem txn db k } |>
  of_result

let find_bind db k ~f =
  match with_read_db db ~f:{ f = fun txn db -> Result.map ~f (Lmdb.get txn db k) } with
  | Error KeyNotFound -> Lwt.return_none
  | Error err -> Lwt.fail_with (Lmdb.string_of_error err)
  | Ok v -> Lwt.return v

module Irmin_value_store
    (M: Irmin.Metadata.S)
    (H: Irmin.Hash.S)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S) = struct

  module XContents = struct

    type nonrec t = t
    type key = H.t
    type value = C.t

    let lmdb_of_key h =
      "contents/" ^ Cstruct.to_string (H.to_raw h)

    let mem db key =
      let key = lmdb_of_key key in
      mem db key

    let find db key =
      let key = lmdb_of_key key in
      find_bind db key ~f:begin fun v ->
        Option.of_result (C.of_string Cstruct.(to_string (of_bigarray v)))
      end

    let to_string = Fmt.to_to_string C.pp

    let add db v =
      let k = H.digest C.t v in
      let k_lmdb = lmdb_of_key k in
      let v = to_string v in
      add db k_lmdb v >|= fun () -> k

    module Val = C
    module Key = H
  end

  module Contents = Irmin.Contents.Store(XContents)

  module XNode = struct
    module Key = H
    module Path = P

    module Val = struct
      module Metadata = M

      type kind = [ `Node | `Contents of M.t ]
      type metadata = M.t
      type entry = { kind : kind; name : string; node : H.t; }
      type t = entry list
      type contents = Contents.key
      type node = Key.t
      type step = Path.step
      type value = [`Node of node | `Contents of contents * metadata ]
      let metadata_t = M.t
      let contents_t = Contents.Key.t
      let node_t = Key.t
      let step_t = Path.step_t

      let entry_t =
        let open Irmin.Type in
        record "Tree.entry"
          (fun kind name node ->
             let kind =
               match kind with
               | None -> `Node
               | Some m -> `Contents m in
             { kind ; name ; node } )
        |+ field "kind" (option M.t) (function
            | { kind = `Node ; _ } -> None
            | { kind = `Contents m ; _ } -> Some m)
        |+ field "name" string (fun { name ; _ } -> name)
        |+ field "node" H.t (fun { node ; _ } -> node)
        |> sealr

      let value_t =
        let open Irmin.Type in
        variant "Tree.value" (fun node contents -> function
            | `Node n          -> node n
            | `Contents (c, m) -> contents (c, m))
        |~ case1 "node"     node_t     (fun n -> `Node n)
        |~ case1 "contents" (pair contents_t M.t) (fun (c, m) -> `Contents (c, m))
        |> sealv

      let of_step = Fmt.to_to_string P.pp_step

      let to_step str = match P.step_of_string str with
        | Ok x           -> x
        | Error (`Msg e) -> failwith e

      let to_entry kind (name, node) =
        { kind; name = of_step name; node }

      let list t  =
        List.fold_left (fun acc { kind; name; node } ->
            let name = to_step name in
            match kind with
            | `Node       -> (name, `Node node) :: acc
            | `Contents m -> (name, `Contents (node, m)) :: acc
          ) [] t
        |> List.rev

      let find t s =
        let s = of_step s in
        let rec aux = function
          | [] -> None
          | x::xs when x.name <> s -> aux xs
          | { kind; node; _ } :: _ ->
              match kind with
              | `Node     -> Some (`Node node)
              | `Contents m -> Some (`Contents (node, m))
        in
        aux t

      type compare_result = LT | EQ | GT

      module Sort_key: sig
        type t
        val of_entry: entry -> t
        val of_contents: string -> t
        val of_node: string -> t
        val order: t -> t -> compare_result
        val compare: t -> t -> int
      end = struct

        type t =
          | Contents: string -> t
          | Node    : string -> t

        exception Result of int

        let str = function Contents s | Node s -> s

        let compare x y = match x, y with
          | Contents x, Contents y -> String.compare x y
          | _  ->
              let xs = str x and ys = str y in
              let lenx = String.length xs in
              let leny = String.length ys in
              let i = ref 0 in
              try
                while !i < lenx && !i < leny do
                  match
                    Char.compare
                      (String.unsafe_get xs !i) (String.unsafe_get ys !i)
                  with
                  | 0 -> incr i
                  | i -> raise (Result i)
                done;
                let get len s i =
                  if i < len then String.unsafe_get (str s) i
                  else if i = len then match s with
                    | Node _     -> '/'
                    | Contents _ -> '\000'
                  else '\000'
                in
                match Char.compare (get lenx x !i) (get leny y !i) with
                | 0 -> Char.compare (get lenx x (!i + 1)) (get leny y (!i + 1))
                | i -> i
              with Result i ->
                i

        let order a b = match compare a b with
          | 0 -> EQ
          | x when x > 0 -> GT
          | _ -> LT

        let of_contents c = Contents c
        let of_node n = Node n

        let of_entry = function
          | {name = n; kind = `Node; _} -> of_node n
          | {name = n; kind = `Contents _; _} -> of_contents n
      end

      let compare_entries a b =
        Sort_key.(compare (of_entry a) (of_entry b))

      (* the order is always:

         [ ...; foo (content key); ...; foo/ (node key);  ... ]

         So always scan until the 'node' key.
      *)

      let remove t step =
        let step = of_step step in
        let node_key = Sort_key.of_node step in
        let contents_key = Sort_key.of_contents step in
        let return ~acc rest = List.rev_append acc rest in
        let rec aux acc = function
          | []     -> t
          | h :: l ->
              let entry_key = Sort_key.of_entry h in
              if Sort_key.order contents_key entry_key = EQ then
                return ~acc l
              else match Sort_key.order node_key entry_key with
                | GT -> aux (h :: acc) l
                | EQ -> return ~acc l
                | LT -> t
        in
        aux [] t

      let hash_of_v = function
        | `Contents (x, _) | `Node x -> x

      let update t step v =
        let step = of_step step in
        let node_key = Sort_key.of_node step in
        let contents_key = Sort_key.of_contents step in
        let return ~acc rest =
          let kind, node = match v with
            | `Node n          -> `Node, n
            | `Contents (c, m) -> `Contents m, c
          in
          let e = { kind; name = step; node} in
          List.rev_append acc (e :: rest)
        in
        let rec aux acc = function
          | [] -> return ~acc []
          | { node; _ } as h :: l ->
              let entry_key = Sort_key.of_entry h in
              (* Remove any contents entry with the same name. This will always
                 come before the new succ entry. *)
              if Sort_key.order contents_key entry_key = EQ then
                aux acc l
              else match Sort_key.order node_key entry_key with
                | GT -> aux (h :: acc) l
                | LT -> return ~acc (h::l)
                | EQ when Cstruct.equal (H.to_raw (hash_of_v v)) (H.to_raw node) -> t
                | EQ -> return ~acc l
        in
        aux [] t

      let empty = []

      let is_empty = function
        | [] -> true
        | _  -> false

      let v alist =
        let alist = List.map (fun (l, x) ->
            let v k = l, k in
            match x with
            | `Node n             -> to_entry `Node (v n)
            | `Contents (c, m) -> to_entry (`Contents m) (v c)
          ) alist
        in
        List.fast_sort compare_entries alist

      let alist t =
        let mk_n k = `Node k in
        let mk_c k m= `Contents (k, m) in
        List.map (function
            | { kind = `Node; name; node } -> (to_step name, mk_n node)
            | { kind = `Contents m; name; node; _ } ->
                (to_step name, mk_c node m)
          ) t

      module N = Irmin.Private.Node.Make (H)(H)(P)(M)
      let to_n t = N.v (alist t)
      let of_n n = v (N.list n)
      let t = Irmin.Type.like N.t of_n to_n
    end

    module AO = struct

      type nonrec t = t
      type key = H.t
      type value = Val.t

      let lmdb_of_key h =
        "node/" ^ Cstruct.to_string (H.to_raw h)

      let mem db key =
        let key = lmdb_of_key key in
        mem db key

      let of_cstruct v =
        Irmin.Type.decode_cstruct (Irmin.Type.list Val.entry_t) v |>
        Option.of_result

      let find db key =
        let key = lmdb_of_key key in
        find_bind db key ~f:(fun v -> of_cstruct (cstruct_of_ba_copy v))

      let add db v =
        let v = Irmin.Type.encode_cstruct (Irmin.Type.list Val.entry_t) v in
        let k = H.digest Irmin.Type.cstruct v in
        let k_lmdb = lmdb_of_key k in
        add_cstruct db k_lmdb v >|= fun () -> k
    end
    include AO

  end
  module Node = Irmin.Private.Node.Store(Contents)(P)(M)(XNode)

  module XCommit = struct
    module Val = struct
      type t = {
        node: H.t ;
        parents: H.t list ;
        info: Irmin.Info.t ;
      }
      type commit = H.t
      type node = H.t

      let commit_t = H.t
      let node_t = H.t

      let v ~info ~node ~parents = { info ; node ; parents }
      let xnode { node; _ } = node
      let node t = xnode t
      let parents { parents; _ } = parents
      let info { info; _ } = info

      module C = Irmin.Private.Commit.Make(H)(H)

      let of_c c = v ~info:(C.info c) ~node:(C.node c) ~parents:(C.parents c)

      let to_c { info ; node ; parents } =
        C.v ~info ~node ~parents

      let t = Irmin.Type.like C.t of_c to_c
    end

    module Key = H

    module AO = struct

      let lmdb_of_key h =
        "commit/" ^ Cstruct.to_string (H.to_raw h)

      type nonrec t = t
      type key = H.t
      type value = Val.t

      let mem db key =
        let key = lmdb_of_key key in
        mem db key

      let of_cstruct v =
        Irmin.Type.decode_cstruct Val.t v |>
        Option.of_result

      let find db key =
        let key = lmdb_of_key key in
        find_bind db key ~f:(fun v -> of_cstruct (cstruct_of_ba_copy v))

      let add db v =
        let v = Irmin.Type.encode_cstruct Val.t v in
        let k = H.digest Irmin.Type.cstruct v in
        let k_lmdb = lmdb_of_key k in
        add_cstruct db k_lmdb v >>= fun () ->
        of_result @@ commit_wtxn db >|= fun () -> k

    end
    include AO

  end
  module Commit = Irmin.Private.Commit.Store(Node)(XCommit)

end

module type Branch = sig
  include Irmin.Branch.S
  val pp_ref: t Fmt.t
  val of_ref: string -> (t, [`Msg of string]) result
end

module Branch (B: Irmin.Branch.S): Branch with type t = B.t = struct
  open Astring
  include B
  let pp_ref ppf b = Fmt.pf ppf "heads/%a" B.pp b

  let of_ref str = match String.cuts ~sep:"/" str with
    | "heads" :: b -> B.of_string (String.concat ~sep:"/" b)
    | _ -> Error (`Msg (Fmt.strf "%s is not a valid branch" str))
end


module Irmin_branch_store (B: Branch) (H: Irmin.Hash.S) = struct

  module Key = B
  module Val = H

  module W = Irmin.Private.Watch.Make(Key)(Val)

  type nonrec t = {
    db: t;
    w: W.t;
  }

  let watches = Hashtbl.create 10

  type key = Key.t
  type value = Val.t
  type watch = W.watch * (unit -> unit Lwt.t)

  (* let branch_of_lmdb r = *)
  (* let str = String.trim @@ Git.Reference.to_raw r in *)
  (* match B.of_ref str with *)
  (* | Ok r           -> Some r *)
  (* | Error (`Msg _) -> None *)

  let lmdb_of_branch r = Fmt.to_to_string B.pp_ref r

  let mem db r =
    mem db.db (lmdb_of_branch r)

  let find db r =
    find_bind db.db (lmdb_of_branch r)
      ~f:(fun v -> Some (H.of_raw (cstruct_of_ba_copy v)))

  let listen_dir _ =
    Lwt.return (fun () -> Lwt.return_unit)

  let watch_key t key ?init f =
    listen_dir t >>= fun stop ->
    W.watch_key t.w key ?init f >|= fun w ->
    (w, stop)

  let watch t ?init f =
    listen_dir t >>= fun stop ->
    W.watch t.w ?init f >|= fun w ->
    (w, stop)

  let unwatch t (w, stop) =
    stop () >>= fun () ->
    W.unwatch t.w w

  let v db (* ~head *) =
    let w =
      try Hashtbl.find watches db.root
      with Not_found ->
        let w = W.v () in
        (* FIXME: we might want to use a weak table *)
        Hashtbl.add watches db.root w;
        w
    in
    Lwt.return { db ; w }

  let list _ = Lwt.return_nil (* TODO, or not *)

  (* let write_index _t _gr _gk = *)
  (* Lwt.return_unit *)

  let set _t r _k =
    Log.debug (fun f -> f "update %a" B.pp r);
    Lwt.return_unit
  (* let gr = git_of_branch r in *)
  (* let gk = git_of_commit k in *)
  (* G.write_reference t.t gr gk >>= fun () -> *)
  (* W.notify t.w r (Some k) >>= fun () -> *)
  (* write_index t gr (Git.Hash.to_commit gk) *)

  let remove _t r =
    Log.debug (fun f -> f "remove %a" B.pp r);
    Lwt.return_unit
  (* G.remove_reference t.t (git_of_branch r) >>= fun () -> *)
  (* W.notify t.w r None *)

  let test_and_set _t _r ~test:_ ~set:_ =
    Log.debug (fun f -> f "test_and_set");
    Lwt.return_true
    (* let gr = git_of_branch r in *)
    (* let c = function None -> None | Some h -> Some (git_of_commit h) in *)
    (* G.test_and_set_reference t.t gr ~test:(c test) ~set:(c set) >>= fun b -> *)
    (* (if b then W.notify t.w r set else Lwt.return_unit) >>= fun () -> *)
    (* begin *)
    (* We do not protect [write_index] because it can take a log
       time and we don't want to hold the lock for too long. Would
       be safer to grab a lock, although the expanded filesystem
       is not critical for Irmin consistency (it's only a
       convenience for the user). *)
    (* if b then match set with *)
    (* | None   -> Lwt.return_unit *)
    (* | Some v -> write_index t gr (Git.Hash.to_commit (git_of_commit v)) *)
    (* else *)
    (* Lwt.return_unit *)
    (* end >|= fun () -> *)
    (* b *)

end


module Make
    (M: Irmin.Metadata.S)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S) = struct

  module P = struct

    module Branch = Irmin_branch_store(Branch(B))(H)
    include Irmin_value_store(M)(H)(C)(P)
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = struct
      type t = unit
      type commit = H.t
      type branch = B.t
      let fetch _ ?depth:_ ~uri:_  _ = Lwt.return_error `Not_available
      let push _ ?depth:_ ~uri:_ _ = Lwt.return_error `Not_available
      let v _ = Lwt.return_unit
    end

    module Repo = struct
      type nonrec t = {
        config: Irmin.config ;
        db: t ;
        branch: Branch.t ;
      }
      let branch_t t : Branch.t = t.branch
      let contents_t t : Contents.t = t.db
      let node_t t : Node.t = contents_t t, t.db
      let commit_t t : Commit.t = node_t t, t.db

      type config = {
        root   : string option ;
        mapsize : int64 ;
        readonly : bool ;
        (* TODO *)
        (* ?write_buffer_size:int -> *)
        (* ?max_open_files:int -> *)
        (* ?block_size:int -> *)
        (* ?block_restart_interval:int -> *)
        (* ?cache_size:int *)
      }

      let config c =
        let root = Irmin.Private.Conf.get c Conf.root in
        let mapsize = Irmin.Private.Conf.get c Conf.mapsize in
        let readonly = Irmin.Private.Conf.get c Conf.readonly in
        { root ; mapsize ; readonly }

      let v conf =
        let { root ; mapsize ; readonly } = config conf in
        let root = match root with None -> "irmin.ldb" | Some root -> root in
        if not (Sys.file_exists root) then Unix.mkdir root 0o755 ;
        let flags = if readonly then [ Lmdb.RdOnly ] else [] in
        let sync_flag =
          match Sys.getenv_opt "TEZOS_CONTEXT_SYNC" with
          | None -> []
          | Some s ->
              match String.lowercase_ascii s with
              | "nosync" -> [ Lmdb.NoSync ]
              | "nometasync" -> [ Lmdb.NoMetaSync ]
              | _ ->
                  Printf.eprintf "Unrecognized TEZOS_CONTEXT_SYNC option : %s\n\
                                  allowed: nosync nometasync" s;
                  []
        in
        let flags = sync_flag @ Lmdb.NoRdAhead :: Lmdb.NoTLS :: flags in
        let file_flags = if readonly then 0o444 else 0o644 in
        match Lmdb.opendir ~mapsize ~flags root file_flags with
        | Error err -> Lwt.fail_with (Lmdb.string_of_error err)
        | Ok db ->
            let db = { db ; root ; wtxn = None } in
            Branch.v db >|= fun branch ->
            { db; branch; config = conf }

    end
  end

  include Irmin.Make_ext(P)

end

include Conf
