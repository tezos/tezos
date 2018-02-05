(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module StringMap = Map.Make(String)

type key = string list
type value = MBytes.t

type t =
  | Dir of t StringMap.t
  | Key of value

let empty = Dir StringMap.empty

let rec raw_get m k =
  match k, m with
  | [], m -> Some m
  | n :: k, Dir m -> begin
      try raw_get (StringMap.find n m) k
      with Not_found -> None
    end
  | _ :: _, Key _ -> None

let rec raw_set m k v =
  match k, m, v with
  | [], (Key _ as m), Some v ->
      if m = v then None else Some v
  | [], (Dir _ as m), Some v ->
      if m == v then None else Some v
  | [], (Key _ | Dir _), None -> Some empty
  | n :: k, Dir m, _ -> begin
      match raw_set (StringMap.find n m) k v with
      | exception Not_found -> begin
          match raw_set empty k v with
          | None -> None
          | Some rm ->
              if rm = empty then
                Some (Dir (StringMap.remove n m))
              else
                Some (Dir (StringMap.add n rm m))
        end
      | None -> None
      | Some rm ->
          if rm = empty then
            Some (Dir (StringMap.remove n m))
          else
            Some (Dir (StringMap.add n rm m))
    end
  | _ :: _, Key _, None -> None
  | _ :: _, Key _, Some _ ->
      Pervasives.failwith "Mem_context.set"

let mem m k =
  match raw_get m k with
  | Some (Key _) -> Lwt.return_true
  | Some (Dir _) | None -> Lwt.return_false

let dir_mem m k =
  match raw_get m k with
  | Some (Dir _) -> Lwt.return_true
  | Some (Key _) | None -> Lwt.return_false

let get m k =
  match raw_get m k with
  | Some (Key v) -> Lwt.return_some v
  | Some (Dir _) | None -> Lwt.return_none

let set m k v =
  match raw_set m k (Some (Key v)) with
  | None -> Lwt.return m
  | Some m -> Lwt.return m
let del m k =
  (* TODO assert key *)
  match raw_set m k None with
  | None -> Lwt.return m
  | Some m -> Lwt.return m
let remove_rec m k =
  match raw_set m k None with
  | None -> Lwt.return m
  | Some m -> Lwt.return m

let fold m k ~init ~f =
  match raw_get m k with
  | None -> Lwt.return init
  | Some (Key _) -> Lwt.return init
  | Some (Dir m) ->
      StringMap.fold
        (fun n m acc ->
           acc >>= fun acc ->
           match m with
           | Key _ -> f (`Key (k @ [n])) acc
           | Dir _ -> f (`Dir (k @ [n])) acc)
        m (Lwt.return init)

let rec pp ppf m =
  match m with
  | Key s -> Format.fprintf ppf "%s" (MBytes.to_string s)
  | Dir m ->
      StringMap.iter
        (fun n m ->
           match m with
           | Key s ->
               Format.fprintf ppf "- %s: %s@ " n (MBytes.to_string s)
           | Dir m ->
               Format.fprintf ppf "- %s:@[<v 2>@ %a@]@ " n pp (Dir m))
        m

let dump m = Format.eprintf "@[<v>%a@]" pp m
