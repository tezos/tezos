(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module List = ListLabels

type t = LevelDB.db
type key = string list
type value = MBytes.t

type error += Unknown of string list

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"raw_store.unknown"
    ~title:"Missing key in store"
    ~description:"Missing key in store"
    ~pp:(fun ppf keys ->
        Format.fprintf ppf
          "Missing key in store: %s"
          (String.concat "/" keys))
    Data_encoding.(obj1 (req "key" (list string)))
    (function Unknown keys -> Some keys | _ -> None)
    (fun keys -> Unknown keys)

let concat = String.concat "/"
let split = String.split_on_char '/'

let init path =
  try
    return (LevelDB.open_db path)
  with exn ->
    Lwt.return (error_exn exn)

let close t = LevelDB.close t

let known t key =
  Lwt.return (LevelDB.mem t (concat key))

let read_opt t key =
  Lwt.return (map_option ~f:MBytes.of_string (LevelDB.get t (concat key)))

let read t key =
  match LevelDB.get t (concat key) with
  | None -> fail (Unknown key)
  | Some k -> return (MBytes.of_string k)

let read_exn t key =
  Lwt.wrap2 LevelDB.get_exn t (concat key) >|= MBytes.of_string

let store t k v =
  LevelDB.put t (concat k) (MBytes.to_string v) ;
  Lwt.return_unit

let remove t k =
  LevelDB.delete t (concat k) ;
  Lwt.return_unit

let is_prefix s s' =
  String.(length s <= length s' && compare s (sub s' 0 (length s)) = 0)

let known_dir t k =
  let ret = ref false in
  let k = concat k in
  LevelDB.iter_from begin fun kk _ ->
    if is_prefix k kk then ret := true ;
    false
  end t k ;
  Lwt.return !ret

let remove_dir t k =
  let k = concat k in
  let batch = LevelDB.Batch.make () in
  LevelDB.iter_from begin fun kk _ ->
    if is_prefix k kk then begin
      LevelDB.Batch.delete batch kk ;
      true
    end
    else false
  end t k ;
  LevelDB.Batch.write t batch ;
  Lwt.return_unit

let list_equal l1 l2 len =
  if len < 0 || len > List.length l1 || len > List.length l2
  then invalid_arg "list_compare: invalid len" ;
  let rec inner l1 l2 len =
    match len, l1, l2 with
    | 0, _, _ -> true
    | _, [], _
    | _, _, [] -> false
    | _, h1 :: t1, h2 :: t2 ->
        if h1 <> h2 then false
        else inner t1 t2 (pred len)
  in
  inner l1 l2 len

let is_child ~parent ~child =
  let plen = List.length parent in
  let clen = List.length child in
  clen > plen && list_equal parent child plen

let list_sub l pos len =
  if len < 0 || pos < 0 || pos + len > List.length l then
    invalid_arg "list_sub" ;
  let rec inner (acc, n) = function
    | [] -> List.rev acc
    | h :: t ->
        if n = 0 then List.rev acc
        else inner (h :: acc, pred n) t in
  inner ([], len) l

let fold t k ~init ~f =
  let k_concat = concat k in
  let base_len = List.length k in
  let i = LevelDB.Iterator.make t in
  LevelDB.Iterator.seek i k_concat 0 (String.length k_concat) ;
  let returned = Hashtbl.create 31 in
  let rec inner acc =
    match LevelDB.Iterator.valid i with
    | false -> Lwt.return acc
    | true ->
        let kk = LevelDB.Iterator.get_key i in
        let kk_split = split kk in
        match is_child ~child:kk_split ~parent:k with
        | false -> Lwt.return acc
        | true ->
            let cur_len = List.length kk_split in
            LevelDB.Iterator.next i ;
            if cur_len = succ base_len then begin
              (f (`Key kk_split) acc) >>= inner
            end
            else begin
              let dir = list_sub kk_split 0 (succ base_len) in
              if Hashtbl.mem returned dir then
                inner acc
              else begin
                Hashtbl.add returned dir () ;
                (f (`Dir dir) acc) >>= inner
              end
            end ;
  in
  inner init

let fold_keys s k ~init ~f =
  let rec loop k acc =
    fold s k ~init:acc
      ~f:(fun file acc ->
          match file with
          | `Key k -> f k acc
          | `Dir k -> loop k acc) in
  loop k init

let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))
