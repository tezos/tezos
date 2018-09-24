(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Option = struct
  let map ~f = function
    | None -> None
    | Some v -> Some (f v)
end

let finalize ~final ~f =
  try
    let res = f () in
    final () ;
    res
  with exn ->
    final () ;
    raise exn

open Rresult

type error =
  | NoSuchFileOrDir
  | IOError
  | EnvironmentLocked
  | OutOfMemory
  | PermissionDenied
  | InvalidArgument
  | NoSpaceLeftOnDevice
  | KeyExist
  | KeyNotFound
  | PageNotFound
  | Corrupted
  | Panic
  | VersionMismatch
  | InvalidFile
  | MapFull
  | DbsFull
  | ReadersFull
  | TLSFull
  | TxnFull
  | CursorFull
  | PageFull
  | MapResized
  | Incompatible
  | BadRslot
  | BadTxn
  | BadValSize
  | BadDbi
  | TxnProblem

let int_of_error = function
  | NoSuchFileOrDir -> 2
  | IOError -> 5
  | EnvironmentLocked -> 11
  | OutOfMemory -> 12
  | PermissionDenied -> 13
  | InvalidArgument -> 22
  | NoSpaceLeftOnDevice -> 28
  | KeyExist -> -30799
  | KeyNotFound -> -30798
  | PageNotFound -> -30797
  | Corrupted -> -30796
  | Panic -> -30795
  | VersionMismatch -> -30794
  | InvalidFile -> -30793
  | MapFull -> -30792
  | DbsFull -> -30791
  | ReadersFull -> -30790
  | TLSFull -> -30789
  | TxnFull -> -30788
  | CursorFull -> -30787
  | PageFull -> -30786
  | MapResized -> -30785
  | Incompatible -> -30784
  | BadRslot -> -30783
  | BadTxn -> -30782
  | BadValSize -> -30781
  | BadDbi -> -30780
  | TxnProblem -> -30779

let error_of_int = function
  | 2      -> NoSuchFileOrDir
  | 5      -> IOError
  | 11     -> EnvironmentLocked
  | 12     -> OutOfMemory
  | 13     -> PermissionDenied
  | 22     -> InvalidArgument
  | 28     -> NoSpaceLeftOnDevice
  | -30799 -> KeyExist
  | -30798 -> KeyNotFound
  | -30797 -> PageNotFound
  | -30796 -> Corrupted
  | -30795 -> Panic
  | -30794 -> VersionMismatch
  | -30793 -> InvalidFile
  | -30792 -> MapFull
  | -30791 -> DbsFull
  | -30790 -> ReadersFull
  | -30789 -> TLSFull
  | -30788 -> TxnFull
  | -30787 -> CursorFull
  | -30786 -> PageFull
  | -30785 -> MapResized
  | -30784 -> Incompatible
  | -30783 -> BadRslot
  | -30782 -> BadTxn
  | -30781 -> BadValSize
  | -30780 -> BadDbi
  | -30779 -> TxnProblem
  | i -> invalid_arg (Printf.sprintf "error_of_int: %d" i)

type version = {
  major : int ;
  minor : int ;
  patch : int ;
}

external version : unit -> version = "stub_mdb_version"
external strerror : int -> string = "stub_mdb_strerror"

let string_of_error error =
  strerror (int_of_error error)

let pp_error ppf err =
  Format.fprintf ppf "%s" (string_of_error err)

let to_msg t = R.error_to_msg ~pp_error t

type t
external create : unit -> (t, int) result = "stub_mdb_env_create"

type flag_env =
  | FixedMap
  | NoSubdir
  | NoSync
  | RdOnly
  | NoMetaSync
  | WriteMap
  | MapAsync
  | NoTLS
  | NoLock
  | NoRdAhead
  | NoMemInit
  | PrevMeta

let int_of_flag_env = function
  | FixedMap -> 0x01
  | NoSubdir -> 0x4000
  | NoSync -> 0x10_000
  | RdOnly -> 0x20_000
  | NoMetaSync -> 0x40_000
  | WriteMap -> 0x80_000
  | MapAsync -> 0x100_000
  | NoTLS -> 0x200_000
  | NoLock -> 0x400_000
  | NoRdAhead -> 0x800_000
  | NoMemInit -> 0x1_000_000
  | PrevMeta -> 0x2_000_000

let flags_env_of_int v =
  List.fold_left begin fun acc flag ->
    if v land (int_of_flag_env flag) <> 0 then flag :: acc else acc
  end []
    [ FixedMap ; NoSubdir ; NoSync ; RdOnly ; NoMetaSync ;
      WriteMap ; MapAsync ; NoTLS ; NoLock ; NoRdAhead ;
      NoMemInit ; PrevMeta ]

type flag_open =
  | ReverseKey
  | DupSort
  | IntegerKey
  | DupFixed
  | IntegerDup
  | ReverseDup
  | Create

let int_of_flag_open = function
  | ReverseKey -> 0x02
  | DupSort -> 0x04
  | IntegerKey -> 0x08
  | DupFixed -> 0x10
  | IntegerDup -> 0x20
  | ReverseDup -> 0x40
  | Create -> 0x40_000

let flags_open_of_int v =
  List.fold_left begin fun acc flag ->
    if v land (int_of_flag_open flag) <> 0 then flag :: acc else acc
  end []
    [ ReverseKey ; DupSort ; IntegerKey ; DupFixed ; IntegerDup ;
      ReverseDup ; Create ]

type flag_put =
  | NoOverwrite
  | NoDupData
  | Current
  | Reserve
  | Append
  | AppendDup
  | Multiple

let int_of_flag_put = function
  | NoOverwrite -> 0x10
  | NoDupData -> 0x20
  | Current -> 0x40
  | Reserve -> 0x10_000
  | Append -> 0x20_000
  | AppendDup -> 0x40_000
  | Multiple -> 0x80_000

let fold_flags int_of_flag flags =
  List.fold_left (fun a flag -> a lor (int_of_flag flag)) 0 flags

let int_of_flags_env = fold_flags int_of_flag_env
let int_of_flags_open = fold_flags int_of_flag_open
let int_of_flags_put = fold_flags int_of_flag_put

let return ?(on_error = fun () -> ()) ret v =
  if ret = 0 then
    Ok v
  else begin
    on_error () ;
    Error (error_of_int ret)
  end

external set_maxreaders : t -> int -> int = "stub_mdb_env_set_maxreaders" [@@noalloc]

let set_maxreaders t readers =
  let ret = set_maxreaders t readers in
  return ret ()

external set_maxdbs : t -> int -> int = "stub_mdb_env_set_maxdbs" [@@noalloc]

let set_maxdbs t dbs =
  let ret = set_maxdbs t dbs in
  return ret ()

external set_mapsize : t -> int64 -> int = "stub_mdb_env_set_mapsize" [@@noalloc]

let set_mapsize t size =
  let ret = set_mapsize t size in
  return ret ()

external opendir :
  t -> string -> int -> Unix.file_perm -> int = "stub_mdb_env_open" [@@noalloc]

external closedir :
  t -> unit = "stub_mdb_env_close" [@@noalloc]

let opendir ?maxreaders ?maxdbs ?mapsize ?(flags=[]) path mode =
  match create () with
  | Error v -> Error (error_of_int v)
  | Ok t ->
      begin match maxreaders with
        | None -> Ok ()
        | Some readers -> set_maxreaders t readers
      end >>= fun () ->
      begin match maxdbs with
        | None -> Ok ()
        | Some dbs -> set_maxdbs t dbs
      end >>= fun () ->
      begin match mapsize with
        | None -> Ok ()
        | Some size -> set_mapsize t size
      end >>= fun () ->
      let ret = opendir t path (int_of_flags_env flags) mode in
      return ret t ~on_error:(fun () -> closedir t)

external copy :
  t -> string -> int -> int = "stub_mdb_env_copy2" [@@noalloc]

let copy ?(compact=false) t path =
  let ret = copy t path (if compact then 0x01 else 0x00) in
  return ret ()

external copyfd :
  t -> Unix.file_descr -> int -> int = "stub_mdb_env_copyfd2" [@@noalloc]

let copyfd ?(compact=false) t fd =
  let ret = copyfd t fd (if compact then 0x01 else 0x00) in
  return ret ()

type stat = {
  psize : int ;
  depth : int ;
  branch_pages : int ;
  leaf_pages : int ;
  overflow_pages : int ;
  entries : int ;
}

external stat : t -> stat = "stub_mdb_env_stat"

type envinfo = {
  mapsize : int ;
  last_pgno : int ;
  last_txnid : int ;
  maxreaders : int ;
  numreaders : int ;
}

external envinfo : t -> envinfo = "stub_mdb_env_info"

external sync : t -> bool -> int = "stub_mdb_env_sync" [@@noalloc]

let sync ?(force=false) t =
  let ret = sync t force in
  return ret ()

external setclear_flags :
  t -> int -> bool -> int = "stub_mdb_env_set_flags" [@@noalloc]

let set_flags t flags =
  let ret = setclear_flags t (int_of_flags_env flags) true in
  return ret ()

let clear_flags t flags =
  let ret = setclear_flags t (int_of_flags_env flags) false in
  return ret ()

external get_flags : t -> int = "stub_mdb_env_get_flags" [@@noalloc]

let get_flags t =
  flags_env_of_int (get_flags t)

external get_path : t -> string = "stub_mdb_env_get_path"
external get_fd : t -> Unix.file_descr = "stub_mdb_env_get_fd" [@@noalloc]
external get_maxreaders : t -> int = "stub_mdb_env_get_maxreaders" [@@noalloc]
external get_maxkeysize : t -> int = "stub_mdb_env_get_maxkeysize" [@@noalloc]

type rawtxn
type ro
type rw
type _ txn =
  | Txn_ro : rawtxn -> ro txn
  | Txn_rw : rawtxn -> rw txn

let rawtxn_of_txn : type a. a txn -> rawtxn = function
  | Txn_ro rawtxn -> rawtxn
  | Txn_rw rawtxn -> rawtxn

external txn_begin :
  t -> int -> rawtxn option -> (rawtxn, int) result = "stub_mdb_txn_begin"

let create_rw_txn ?(nosync=false) ?(nometasync=false) ?parent t =
  let flags = match nosync, nometasync with
    | true, true -> int_of_flags_env [NoSync; NoMetaSync]
    | true, false -> int_of_flag_env NoSync
    | false, true -> int_of_flag_env NoMetaSync
    | _ -> 0 in
  match txn_begin t flags (Option.map ~f:rawtxn_of_txn parent) with
  | Error i -> Error (error_of_int i)
  | Ok tx -> Ok (Txn_rw tx)

let create_ro_txn ?(nosync=false) ?(nometasync=false) ?parent t =
  let flags = match nosync, nometasync with
    | true, true -> int_of_flags_env [RdOnly; NoSync; NoMetaSync]
    | true, false -> int_of_flags_env [RdOnly; NoSync]
    | false, true -> int_of_flags_env [RdOnly; NoMetaSync]
    | _ -> int_of_flag_env RdOnly in
  match txn_begin t flags (Option.map ~f:rawtxn_of_txn parent) with
  | Error i -> Error (error_of_int i)
  | Ok tx -> Ok (Txn_ro tx)

external get_txn_id : rawtxn -> int = "stub_mdb_txn_id" [@@noalloc]
external get_txn_env : rawtxn -> t = "stub_mdb_txn_env"

let get_txn_id txn =
  get_txn_id (rawtxn_of_txn txn)

let get_txn_env txn =
  get_txn_env (rawtxn_of_txn txn)

external commit_txn : rawtxn -> int = "stub_mdb_txn_commit" [@@noalloc]
external abort_txn : rawtxn -> unit = "stub_mdb_txn_abort" [@@noalloc]

let commit_txn txn =
  return (commit_txn (rawtxn_of_txn txn)) ()

let abort_txn txn =
  abort_txn (rawtxn_of_txn txn)

external reset_ro_txn : rawtxn -> unit = "stub_mdb_txn_reset" [@@noalloc]
external renew_ro_txn : rawtxn -> int = "stub_mdb_txn_renew" [@@noalloc]

let reset_ro_txn (Txn_ro rawtxn) =
  reset_ro_txn rawtxn

let renew_ro_txn (Txn_ro rawtxn) =
  return (renew_ro_txn rawtxn) ()

type db = nativeint

external opendb :
  rawtxn -> string option -> int -> (db, int) result = "stub_mdb_dbi_open"

let opendb ?(flags=[]) ?name txn =
  R.reword_error error_of_int
    (opendb (rawtxn_of_txn txn) name (int_of_flags_open flags))

external db_stat :
  rawtxn -> db -> (stat, int) result = "stub_mdb_stat"

let db_stat txn dbi =
  R.reword_error error_of_int (db_stat (rawtxn_of_txn txn) dbi)

external db_flags :
  rawtxn -> db -> (int, int) result = "stub_mdb_dbi_flags"

let db_flags txn dbi =
  match db_flags (rawtxn_of_txn txn) dbi with
  | Error i -> Error (error_of_int i)
  | Ok v -> Ok (flags_open_of_int v)

external db_drop :
  rawtxn -> db -> bool -> int = "stub_mdb_drop" [@@noalloc]

let db_drop txn dbi =
  return (db_drop (rawtxn_of_txn txn) dbi false) ()

let with_ro_db ?nosync ?nometasync ?parent ?flags ?name t ~f =
  create_ro_txn ?nosync ?nometasync ?parent t >>= fun txn ->
  opendb ?flags ?name txn >>= fun db ->
  match f txn db with
  | exception exn ->
      abort_txn txn ;
      raise exn
  | Ok res ->
      commit_txn txn >>= fun () ->
      Ok res
  | Error err ->
      abort_txn txn ;
      Error err

let with_rw_db ?nosync ?nometasync ?parent ?flags ?name t ~f =
  create_rw_txn ?nosync ?nometasync ?parent t >>= fun txn ->
  opendb ?flags ?name txn >>= fun db ->
  match f txn db with
  | exception exn ->
      abort_txn txn ;
      raise exn
  | Ok res ->
      commit_txn txn >>= fun () ->
      Ok res
  | Error err ->
      abort_txn txn ;
      Error err

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external get :
  rawtxn -> db -> string -> (buffer, int) result = "stub_mdb_get"

let get txn dbi k =
  R.reword_error error_of_int (get (rawtxn_of_txn txn) dbi k)

let mem txn dbi k =
  match get txn dbi k with
  | Ok _ -> Ok true
  | Error KeyNotFound -> Ok false
  | Error err -> Error err

external put :
  rawtxn -> db -> string -> buffer -> int -> int = "stub_mdb_put" [@@noalloc]
external put_string :
  rawtxn -> db -> string -> string -> int -> int = "stub_mdb_put_string" [@@noalloc]

let put ?(flags=[]) txn dbi k v =
  let flags = int_of_flags_put flags in
  return (put (rawtxn_of_txn txn) dbi k v flags) ()

let put_string ?(flags=[]) txn dbi k v =
  let flags = int_of_flags_put flags in
  return (put_string (rawtxn_of_txn txn) dbi k v flags) ()

external del :
  rawtxn -> db -> string -> buffer option -> int = "stub_mdb_del" [@@noalloc]
external del_string :
  rawtxn -> db -> string -> string option -> int = "stub_mdb_del_string" [@@noalloc]

let del ?data txn dbi k =
  return (del (rawtxn_of_txn txn) dbi k data) ()

let del_string ?data txn dbi k =
  return (del_string (rawtxn_of_txn txn) dbi k data) ()

type rawcursor
type _ cursor =
  | Cursor_ro : rawcursor -> ro cursor
  | Cursor_rw : rawcursor -> rw cursor

let rawcursor_of_cursor : type a. a cursor -> rawcursor = function
  | Cursor_ro rawcursor -> rawcursor
  | Cursor_rw rawcursor -> rawcursor

let cursor_ro rawcursor = Cursor_ro rawcursor
let cursor_rw rawcursor = Cursor_rw rawcursor

external opencursor :
  rawtxn -> db -> (rawcursor, int) result = "stub_mdb_cursor_open"

let opencursor :
  type a. a txn -> db -> (a cursor, error) result = fun txn dbi ->
  match txn with
  | Txn_ro rawtxn ->
      R.reword_error error_of_int (opencursor rawtxn dbi) |>
      R.map cursor_ro
  | Txn_rw rawtxn ->
      R.reword_error error_of_int (opencursor rawtxn dbi) |>
      R.map cursor_rw

external cursor_close :
  rawcursor -> unit = "stub_mdb_cursor_close" [@@noalloc]

external cursor_renew :
  rawtxn -> rawcursor -> int = "stub_mdb_cursor_renew" [@@noalloc]

let cursor_close cursor =
  cursor_close (rawcursor_of_cursor cursor)

let cursor_renew (Txn_ro rawtxn) (Cursor_ro rawcursor) =
  return (cursor_renew rawtxn rawcursor) ()

external cursor_txn :
  rawcursor -> rawtxn = "stub_mdb_cursor_txn"

let cursor_txn : type a. a cursor -> a txn = function
  | Cursor_ro rawcursor -> Txn_ro (cursor_txn rawcursor)
  | Cursor_rw rawcursor -> Txn_rw (cursor_txn rawcursor)

external cursor_db :
  rawcursor -> db = "stub_mdb_cursor_dbi" [@@noalloc]

let cursor_db cursor =
  cursor_db (rawcursor_of_cursor cursor)

type cursor_op =
  | First
  | First_dup
  | Get_both
  | Get_both_range
  | Get_current
  | Get_multiple
  | Last
  | Last_dup
  | Next
  | Next_dup
  | Next_multiple
  | Next_nodup
  | Prev
  | Prev_dup
  | Prev_nodup
  | Set
  | Set_key
  | Set_range
  | Prev_multiple

external cursor_get_op :
  rawcursor -> string option -> buffer option -> cursor_op ->
  (buffer * buffer, int) result = "stub_mdb_cursor_get"

let cursor_get_op ?key ?data cursor op =
  R.reword_error error_of_int
    (cursor_get_op (rawcursor_of_cursor cursor) key data op)

let cursor_first cursor =
  R.map ignore (cursor_get_op cursor First)
let cursor_last cursor =
  R.map ignore (cursor_get_op cursor Last)
let cursor_next cursor =
  R.map ignore (cursor_get_op cursor Next)
let cursor_prev cursor =
  R.map ignore (cursor_get_op cursor Prev)
let cursor_at cursor = function
  | "" -> cursor_first cursor
  | key -> R.map ignore (cursor_get_op ~key cursor Set_range)

let cursor_get cursor =
  cursor_get_op cursor Get_current

let cursor_fold_left ~f ~init cursor =
  let rec inner a =
    match cursor_get cursor with
    | Error KeyNotFound -> Ok a
    | Error err -> Error err
    | Ok kv ->
        f a kv >>= fun a ->
        match cursor_next cursor with
        | Error KeyNotFound -> Ok a
        | Error err -> Error err
        | Ok () -> inner a
  in
  inner init

let cursor_iter ~f cursor =
  cursor_fold_left ~init:() ~f:(fun () kv -> f kv) cursor

external cursor_put :
  rawcursor -> string -> buffer -> int -> int = "stub_mdb_cursor_put" [@@noalloc]
external cursor_put_string :
  rawcursor -> string -> string -> int -> int = "stub_mdb_cursor_put_string" [@@noalloc]
external cursor_del :
  rawcursor -> int -> int = "stub_mdb_cursor_del" [@@noalloc]
external cursor_count :
  rawcursor -> (int, int) result = "stub_mdb_cursor_count"

let cursor_put ?(flags=[]) cursor k v =
  return
    (cursor_put (rawcursor_of_cursor cursor) k v (int_of_flags_put flags))
    ()

let cursor_put_string ?(flags=[]) cursor k v =
  return
    (cursor_put_string (rawcursor_of_cursor cursor) k v (int_of_flags_put flags))
    ()

let cursor_del ?(flags=[]) cursor =
  return
    (cursor_del (rawcursor_of_cursor cursor) (int_of_flags_put flags))
    ()

let cursor_count cursor =
  R.reword_error error_of_int
    (cursor_count (rawcursor_of_cursor cursor))

let with_cursor txn db ~f =
  opencursor txn db >>= fun cursor ->
  finalize
    ~final:(fun () -> cursor_close cursor)
    ~f:(fun () -> f cursor)

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
