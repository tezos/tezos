(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Sets as hash tables. Code adapted from ocaml's Hashtbl. *)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type 'a t =
  { mutable size: int;            (* number of elements *)
    mutable data: 'a list array } (* the buckets *)

let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s [] }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- []
  done;
  h.size <- 0

let copy h =
  { size = h.size;
    data = Array.copy h.data }

let resize hashfun tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize [] in
    let rec insert_bucket = function
        [] -> ()
      | key :: rest ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = (hashfun key) mod nsize in
          ndata.(nidx) <- key :: ndata.(nidx) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

let add h key =
  let i = (Hashtbl.hash key) mod (Array.length h.data) in
  let bucket = h.data.(i) in
  if not (List.mem key bucket) then begin
    h.data.(i) <- key :: bucket;
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize Hashtbl.hash h
  end

let remove h key =
  let rec remove_bucket = function
      [] ->
        []
    | k :: next ->
        if k = key
        then begin h.size <- pred h.size; next end
        else k :: remove_bucket next in
  let i = (Hashtbl.hash key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let mem h key =
  List.mem key h.data.((Hashtbl.hash key) mod (Array.length h.data))

let cardinal h =
  let d = h.data in
  let c = ref 0 in
  for i = 0 to Array.length d - 1 do
    c := !c + List.length d.(i)
  done;
  !c

let iter f h =
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    List.iter f d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      [] ->
        accu
    | k :: rest ->
        do_bucket rest (f k accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type elt
    type t
    val create: int -> t
    val clear: t -> unit
    val copy: t -> t
    val add: t -> elt -> unit
    val remove: t -> elt -> unit
    val mem : t -> elt -> bool
    val cardinal: t -> int
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

module Make(H: HashedType): (S with type elt = H.t) =
  struct
    type elt = H.t
    type set = elt t
    type t = set
    let create = create
    let clear = clear
    let copy = copy

    let safehash key = (H.hash key) land max_int

    let rec mem_in_bucket key = function
      | [] -> false
      | x :: r -> H.equal key x || mem_in_bucket key r

    let add h key =
      let i = (safehash key) mod (Array.length h.data) in
      let bucket = h.data.(i) in
      if not (mem_in_bucket key bucket) then begin
	h.data.(i) <- key :: bucket;
	h.size <- succ h.size;
	if h.size > Array.length h.data lsl 1 then resize safehash h
      end

    let remove h key =
      let rec remove_bucket = function
          [] ->
            []
        | k :: next ->
            if H.equal k key
            then begin h.size <- pred h.size; next end
            else k :: remove_bucket next in
      let i = (safehash key) mod (Array.length h.data) in
      h.data.(i) <- remove_bucket h.data.(i)

    let mem h key =
      mem_in_bucket key h.data.((safehash key) mod (Array.length h.data))

    let cardinal = cardinal

    let iter = iter
    let fold = fold
  end
