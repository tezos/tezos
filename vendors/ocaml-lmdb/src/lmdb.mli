(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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

val string_of_error : error -> string
val pp_error : Format.formatter -> error -> unit
val to_msg : ('a, error) result -> ('a, [> R.msg]) result

type version = {
  major : int ;
  minor : int ;
  patch : int ;
}

val version : unit -> version

type ro
type rw
type t

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

val opendir :
  ?maxreaders:int -> ?maxdbs:int -> ?mapsize:int64 -> ?flags:flag_env list ->
  string -> Unix.file_perm -> (t, error) result

val closedir : t -> unit

val copy : ?compact:bool -> t -> string -> (unit, error) result
val copyfd : ?compact:bool -> t -> Unix.file_descr -> (unit, error) result

type stat = {
  psize : int ;
  depth : int ;
  branch_pages : int ;
  leaf_pages : int ;
  overflow_pages : int ;
  entries : int ;
}

val stat : t -> stat

type envinfo = {
  mapsize : int ;
  last_pgno : int ;
  last_txnid : int ;
  maxreaders : int ;
  numreaders : int ;
}

val envinfo : t -> envinfo

val sync : ?force:bool -> t -> (unit, error) result

val get_flags : t -> flag_env list
val set_flags : t -> flag_env list -> (unit, error) result
val clear_flags : t -> flag_env list -> (unit, error) result

val get_path : t -> string
val get_fd : t -> Unix.file_descr

val get_maxreaders : t -> int
val get_maxkeysize : t -> int

val set_mapsize : t -> int64 -> (unit, error) result

type _ txn

val create_rw_txn :
  ?nosync:bool -> ?nometasync:bool ->
  ?parent:rw txn -> t -> (rw txn, error) result

val create_ro_txn :
  ?nosync:bool -> ?nometasync:bool ->
  ?parent:_ txn -> t -> (ro txn, error) result

val get_txn_id : _ txn -> int
val get_txn_env : _ txn -> t

val commit_txn : _ txn -> (unit, error) result
val abort_txn : _ txn -> unit

val reset_ro_txn : ro txn -> unit
val renew_ro_txn : ro txn -> (unit, error) result

type flag_open =
  | ReverseKey
  | DupSort
  | IntegerKey
  | DupFixed
  | IntegerDup
  | ReverseDup
  | Create

type db

val opendb :
  ?flags:flag_open list -> ?name:string -> _ txn -> (db, error) result

val db_stat : _ txn -> db -> (stat, error) result
val db_flags : _ txn -> db -> (flag_open list, error) result
val db_drop : _ txn -> db -> (unit, error) result

val with_ro_db :
  ?nosync:bool -> ?nometasync:bool ->
  ?parent:_ txn -> ?flags:flag_open list ->
  ?name:string -> t -> f:(ro txn -> db -> ('a, error) result) ->
  ('a, error) result

val with_rw_db :
  ?nosync:bool -> ?nometasync:bool ->
  ?parent:rw txn -> ?flags:flag_open list ->
  ?name:string -> t -> f:(rw txn -> db -> ('a, error) result) ->
  ('a, error) result

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val get : _ txn -> db -> string -> (buffer, error) result
val mem : _ txn -> db -> string -> (bool, error) result

type flag_put =
  | NoOverwrite
  | NoDupData
  | Current
  | Reserve
  | Append
  | AppendDup
  | Multiple

val put : ?flags:flag_put list ->
  rw txn -> db -> string -> buffer -> (unit, error) result
val put_string : ?flags:flag_put list ->
  rw txn -> db -> string -> string -> (unit, error) result

val del : ?data:buffer ->
  rw txn -> db -> string -> (unit, error) result
val del_string : ?data:string ->
  rw txn -> db -> string -> (unit, error) result

type _ cursor

val opencursor : 'a txn -> db -> ('a cursor, error) result
val cursor_close : _ cursor -> unit
val cursor_renew : ro txn -> ro cursor -> (unit, error) result

val cursor_txn : 'a cursor -> 'a txn
val cursor_db : _ cursor -> db

val cursor_first : _ cursor -> (unit, error) result
val cursor_last : _ cursor -> (unit, error) result
val cursor_prev : _ cursor -> (unit, error) result
val cursor_next : _ cursor -> (unit, error) result
val cursor_at : _ cursor -> string -> (unit, error) result

val cursor_get : _ cursor -> (buffer * buffer, error) result

val cursor_fold_left :
  f:('a -> (buffer * buffer) -> ('a, error) result) ->
  init:'a -> _ cursor -> ('a, error) result

val cursor_iter :
  f:(buffer * buffer -> (unit, error) result) -> _ cursor -> (unit, error) result

val with_cursor :
  'a txn -> db -> f:('a cursor -> ('b, error) result) ->
  ('b, error) result

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

val cursor_put : ?flags:flag_put list ->
  rw cursor -> string -> buffer -> (unit, error) result
val cursor_put_string : ?flags:flag_put list ->
  rw cursor -> string -> string -> (unit, error) result
val cursor_del : ?flags:flag_put list -> rw cursor -> (unit, error) result
val cursor_count : _ cursor -> (int, error) result

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
