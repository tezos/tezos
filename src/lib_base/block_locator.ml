(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

type t = raw

(** Non private version of Block_store_locator.t for coercions *)
and raw = Block_header.t * Block_hash.t list

let raw x = x

let pp ppf (hd, h_lst) =
  let repeats = 10 in
  let coef = 2 in
  (* list of hashes *)
  let rec pp_hash_list ppf (h_lst , acc , d , r) =
    match h_lst with
    | [] ->
        Format.fprintf ppf ""
    | hd :: tl ->
        let new_d = if r > 1 then d else d * coef in
        let new_r = if r > 1 then r - 1 else repeats in
        Format.fprintf ppf "%a (%i)\n%a" Block_hash.pp hd acc pp_hash_list (tl , acc - d , new_d , new_r) in
  Format.fprintf ppf "%a (head)\n%a"
    Block_hash.pp (Block_header.hash hd)
    pp_hash_list (h_lst , -1, 1, repeats - 1)

let pp_short ppf (hd, h_lst) =
  Format.fprintf ppf "head: %a, %d predecessors"
    Block_hash.pp (Block_header.hash hd)
    (List.length h_lst)

let encoding =
  let open Data_encoding in
  (* TODO add a [description] *)
  (obj2
     (req "current_head" (dynamic_size Block_header.encoding))
     (req "history" (dynamic_size (list Block_hash.encoding))))

(** Computes a locator for block [b] picking 10 times the immediate
    predecessors of [b], then 10 times one predecessor every 2, then
    10 times one predecessor every 4, ..., until genesis or it reaches
    the desired size. *)
let compute ~predecessor ~genesis b header size =
  if size < 0 then
    invalid_arg "Block_locator.compute: negative size"
  else
    let repeats = 10 in (* number of repetitions for each power of 2 *)
    let rec loop acc size step cnt b =
      if size = 0 then
        Lwt.return (List.rev acc)
      else
        predecessor b step >>= function
        | None ->      (* reached genesis before size *)
            if Block_hash.equal b genesis then
              Lwt.return (List.rev acc)
            else
              Lwt.return (List.rev (genesis :: acc))
        | Some pred ->
            if cnt = 1 then
              loop (pred :: acc) (size - 1) (step * 2) repeats pred
            else
              loop (pred :: acc) (size - 1) step (cnt - 1) pred in
    if size = 0 then
      Lwt.return (header, [])
    else
      predecessor b 1 >>= function
      | None -> Lwt.return (header, [])
      | Some p ->
          loop [p] (size-1) 1 (repeats-1) p >>= fun hist ->
          Lwt.return (header, hist)

type validity =
  | Unknown
  | Known_valid
  | Known_invalid

let unknown_prefix cond (head, hist) =
  let rec loop hist acc =
    match hist with
    | [] -> Lwt.return_none
    | h :: t ->
        cond h >>= function
        | Known_valid ->
            Lwt.return_some (h, (List.rev (h :: acc)))
        | Known_invalid ->
            Lwt.return_none
        | Unknown ->
            loop t (h :: acc)
  in
  cond (Block_header.hash head) >>= function
  | Known_valid ->
      Lwt.return_some (Block_header.hash head, (head, []))
  | Known_invalid ->
      Lwt.return_none
  | Unknown ->
      loop hist [] >>= function
      | None ->
          Lwt.return_none
      | Some (tail, hist) ->
          Lwt.return_some (tail, (head, hist))
