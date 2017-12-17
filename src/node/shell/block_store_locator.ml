(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Block_header.t * Block_hash.t list

let encoding =
  let open Data_encoding in
  (* TODO add a [description] *)
  (obj2
     (req "current_head" (dynamic_size Block_header.encoding))
     (req "history" (dynamic_size (list Block_hash.encoding))))

let predecessor (store : Store.Block.store) (b: Block_hash.t) =
  Store.Block.Contents.read_exn (store, b) >>= fun contents ->
  let predecessor = contents.header.shell.predecessor in
  if Block_hash.equal b predecessor then
    Lwt.return_none
  else
    Lwt.return_some predecessor

let compute (store : Store.Block.store) (b: Block_hash.t) sz =
  let rec loop acc ~sz step cpt b =
    if sz = 0 then
      Lwt.return (List.rev acc)
    else
      predecessor store b >>= function
      | None ->
          Lwt.return (List.rev (b :: acc))
      | Some predecessor ->
          if cpt = 0 then
            loop (b :: acc) ~sz:(sz - 1)
              (step * 2) (step * 20 - 1) predecessor
          else if cpt mod step = 0 then
            loop (b :: acc) ~sz:(sz - 1)
              step (cpt - 1) predecessor
          else
            loop acc ~sz step (cpt - 1) predecessor in
  Store.Block.Contents.read_exn (store, b) >>= fun { header } ->
  predecessor store b >>= function
  | None -> Lwt.return (header, [])
  | Some p ->
      loop [] ~sz 1 9 p >>= fun hist ->
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
