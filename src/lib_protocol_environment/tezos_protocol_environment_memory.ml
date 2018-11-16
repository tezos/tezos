(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Context = struct

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
        match StringMap.find_opt n m with
        | Some res -> raw_get res k
        | None -> None
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
        match raw_set (Option.unopt ~default:empty
                         (StringMap.find_opt n m)) k v with
        | None -> None
        | Some rm when rm = empty ->
            Some (Dir (StringMap.remove n m))
        | Some rm ->
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
  let copy m ~from ~to_ =
    match raw_get m from with
    | None -> Lwt.return_none
    | Some v -> Lwt.return (raw_set m to_ (Some v))

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

  let current_protocol_key = ["protocol"]

  let get_protocol v =
    raw_get v current_protocol_key |> function
    | Some (Key data) -> Lwt.return (Protocol_hash.of_bytes_exn data)
    | _ -> assert false

  let set_protocol v key =
    raw_set v current_protocol_key (Some (Key (Protocol_hash.to_bytes key))) |> function
    | Some m -> Lwt.return m
    | None -> assert false


  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c

end

include Tezos_protocol_environment.Make(Context)
