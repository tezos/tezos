(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Dynamic Ledger Solutions <contact@tezos.com>
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

(** Quick-and-dirty LevelDB backend for Irmin. *)

val config:
  ?config:Irmin.config -> ?mapsize:int64 -> ?readonly:bool -> string -> Irmin.config

module Make
  (M: Irmin.Metadata.S)
  (C: Irmin.Contents.S)
  (P: Irmin.Path.S)
  (B: Irmin.Branch.S)
  (H: Irmin.Hash.S): sig
  include Irmin.S
    with type key = P.t
     and type step = P.step
     and module Key = P
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type Commit.Hash.t = H.t
     and type Tree.Hash.t = H.t
     and type Contents.Hash.t = H.t

  type stats

  val promoted_contents: stats -> int
  val promoted_nodes: stats -> int
  val promoted_commits: stats -> int

  val pp_stats: stats Fmt.t

  val gc:
    repo:Repo.t ->
    ?before_pivot:(unit -> unit Lwt.t) ->
    ?branches:B.t list ->
    ?switch:Lwt_switch.t ->
    Commit.hash list -> stats Lwt.t

end
