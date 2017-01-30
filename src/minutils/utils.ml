(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module StringMap = Map.Make (String)

let split delim ?(dup = true) ?(limit = max_int) path =
  let l = String.length path in
  let rec do_slashes acc limit i =
    if i >= l then
      List.rev acc
    else if String.get path i = delim then
      if dup then
        do_slashes acc limit (i + 1)
      else
        do_split acc limit (i + 1)
    else
      do_split acc limit i
  and do_split acc limit i =
    if limit <= 0 then
      if i = l then
        List.rev acc
      else
        List.rev (String.sub path i (l - i) :: acc)
    else
      do_component acc (pred limit) i i
  and do_component acc limit i j =
    if j >= l then
      if i = j then
        List.rev acc
      else
        List.rev (String.sub path i (j - i) :: acc)
    else if String.get path j = delim then
      do_slashes (String.sub path i (j - i) :: acc) limit j
    else
      do_component acc limit i (j + 1) in
  if limit > 0 then
    do_slashes [] limit 0
  else
    [ path ]

let split_path path = split '/' path

let map_option ~f = function
  | None -> None
  | Some x -> Some (f x)

let apply_option ~f = function
  | None -> None
  | Some x -> f x

let iter_option ~f = function
  | None -> ()
  | Some x -> f x

let unopt ~default = function
  | None -> default
  | Some x -> x

let unopt_map ~f ~default = function
  | None -> default
  | Some x -> f x

let unopt_list l =
  let may_cons xs x = match x with None -> xs | Some x -> x :: xs in
  List.rev @@ List.fold_left may_cons [] l

let first_some a b = match a, b with
  | None, None -> None
  | None, Some v -> Some v
  | Some v, _ -> Some v

let filter_map f l =
  let may_cons xs x = match f x with None -> xs | Some x -> x :: xs in
  List.rev @@ List.fold_left may_cons [] l

let list_rev_sub l n =
  ListLabels.fold_left l ~init:(n, []) ~f:begin fun (n, l) elt ->
    if n <= 0 then (n, l) else (n - 1, elt :: l)
  end |> snd

let list_sub l n = list_rev_sub l n |> List.rev

let list_hd_opt = function
  | [] -> None
  | h :: _ -> Some h

let display_paragraph ppf description =
  Format.fprintf ppf "@[%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline
       (fun ppf line ->
          Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf w ->
               (* replace &nbsp; by real spaces... *)
               Format.fprintf ppf "%s@ "
                 (Stringext.replace_all ~pattern:"\xC2\xA0" ~with_:" " w))
            ppf
            (split ' ' line)))
    (split ~dup:false '\n' description)

let rec remove_elem_from_list nb = function
  | [] -> []
  | l when nb <= 0 -> l
  | _ :: tl -> remove_elem_from_list (nb - 1) tl

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if n >= x && String.sub s 0 x = prefix then
    Some (String.sub s x (n - x))
  else
    None

let finalize f g = try let res = f () in g (); res with exn -> g (); raise exn

let read_file ?(bin=false) fn =
  let ic = (if bin then open_in_bin else open_in) fn in
  finalize (fun () ->
      let len = in_channel_length ic in
      let buf = Bytes.create len in
      let nb_read = input ic buf 0 len in
      if nb_read <> len then failwith (Printf.sprintf "read_file: read %d, expected %d" nb_read len)
      else Bytes.unsafe_to_string buf)
    (fun () -> close_in ic)

let write_file ?(bin=false) fn contents =
  let oc = (if bin then open_out_bin else open_out) fn in
  finalize (fun () ->
      let contents = Bytes.unsafe_of_string contents in
      output oc contents 0 @@ Bytes.length contents
    )
    (fun () -> close_out oc)

let (<<) g f = fun a -> g (f a)

let rec (--) i j =
  let rec loop acc j =
    if j < i then acc else loop (j :: acc) (pred j) in
  loop [] j

let rec repeat n x = if n <= 0 then [] else x :: repeat (pred n) x

let take_n_unsorted n l =
  let rec loop acc n = function
    | [] -> l
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> loop (x :: acc) (pred n) xs in
  loop [] n l

module Bounded(E: Set.OrderedType) = struct

  (* TODO one day replace list by an heap array *)

  type t = {
    bound : int ;
    mutable size : int ;
    mutable data : E.t list ;
  }
  let create bound = { bound ; size = 0 ; data = [] }

  let rec push x = function
    | [] -> [x]
    | (y :: xs) as ys ->
        let c = compare x y in
        if c < 0 then x :: ys else if c = 0 then ys else y :: push x xs

  let replace x xs =
    match xs with
    | y :: xs when compare x y > 0 ->
        push x xs
    | xs -> xs

  let insert x t =
    if t.size < t.bound then begin
      t.size <- t.size + 1 ;
      t.data <- push x t.data
    end else if E.compare (List.hd t.data) x < 0 then
      t.data <- replace x t.data

  let get { data } = data

end

let take_n_sorted (type a) compare n l =
  let module B = Bounded(struct type t = a let compare = compare end) in
  let t = B.create n in
  List.iter (fun x -> B.insert x t) l ;
  B.get t

let take_n ?compare n l =
  match compare with
  | None -> take_n_unsorted n l
  | Some compare -> take_n_sorted compare n l

let select n l =
  let rec loop n acc = function
    | [] -> invalid_arg "Utils.select"
    | x :: xs when n <= 0 -> x, List.rev_append acc xs
    | x :: xs -> loop (pred n) (x :: acc) xs
  in
  loop n [] l


let mem_char s c =
  match String.index s c with
  | exception Not_found -> false
  | _ -> true

let check_port port =
  if mem_char port '[' || mem_char port ']' || mem_char port ':' then
    invalid_arg "Utils.parse_addr_port (invalid character in port)"

let parse_addr_port s =
  let len = String.length s in
  if len = 0 then
    ("", "")
  else if s.[0] = '[' then begin (* inline IPv6 *)
    match String.rindex s ']' with
    | exception Not_found ->
        invalid_arg "Utils.parse_addr_port (missing ']')"
    | pos ->
        let addr = String.sub s 1 (pos - 1) in
        let port =
          if pos = len - 1 then
            ""
          else if s.[pos+1] <> ':' then
            invalid_arg "Utils.parse_addr_port (unexpected char after ']')"
          else
            String.sub s (pos + 2) (len - pos - 2) in
        check_port port ;
        addr, port
  end else begin
    match String.rindex s ']' with
    | _pos ->
        invalid_arg "Utils.parse_addr_port (unexpected char ']')"
    | exception Not_found ->
        match String.index s ':' with
        | exception _ -> s, ""
        | pos ->
            match String.index_from s (pos+1) ':'  with
            | exception _ ->
                let addr = String.sub s 0 pos in
                let port = String.sub s (pos + 1) (len - pos - 1) in
                check_port port ;
                addr, port
            | _pos ->
                invalid_arg "split_url_port: IPv6 addresses must be bracketed"
  end
