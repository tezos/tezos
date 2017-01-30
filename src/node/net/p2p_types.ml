(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Net

module Canceler = Lwt_utils.Canceler

module Version = struct
  type t = {
    name : string ;
    major : int ;
    minor : int ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { name; major; minor } -> (name, major, minor))
      (fun (name, major, minor) -> { name; major; minor })
      (obj3
         (req "name" string)
         (req "major" int8)
         (req "minor" int8))

  (* the common version for a pair of peers, if any, is the maximum one,
     in lexicographic order *)
  let common la lb =
    let la = List.sort (fun l r -> compare r l) la in
    let lb = List.sort (fun l r -> compare r l) lb in
    let rec find = function
      | [], _ | _, [] -> None
      | ((a :: ta) as la), ((b :: tb) as lb) ->
          if a = b then Some a
          else if a < b then find (ta, lb)
          else find (la, tb)
    in find (la, lb)
end

module Stat = struct

  type t = {
    total_sent : int ;
    total_recv : int ;
    current_inflow : int ;
    current_outflow : int ;
  }

  let print_size ppf sz =
    let ratio n = (float_of_int sz /. float_of_int (1 lsl n)) in
    if sz < 1 lsl 10 then
      Format.fprintf ppf "%d B" sz
    else if sz < 1 lsl 20 then
      Format.fprintf ppf "%.2f kiB" (ratio 10)
    else
      Format.fprintf ppf "%.2f MiB" (ratio 20)

  let pp ppf stat =
    Format.fprintf ppf
      "sent: %a (%a/s) recv: %a (%a/s)"
      print_size stat.total_sent print_size stat.current_outflow
      print_size stat.total_recv print_size stat.current_inflow

end

module Gid = struct
  include Crypto_box.Public_key_hash
  let pp = pp_short
  module Map = Map.Make (Crypto_box.Public_key_hash)
  module Set = Set.Make (Crypto_box.Public_key_hash)
  module Table = Hash.Hash_table (Crypto_box.Public_key_hash)
end

(* public types *)
type addr = Ipaddr.V6.t
type port = int

module Point = struct

  module T = struct

    (* A net point (address x port). *)
    type t = addr * port
    let compare (a1, p1) (a2, p2) =
      match Ipaddr.V6.compare a1 a2 with
      | 0 -> p1 - p2
      | x -> x
    let equal p1 p2 = compare p1 p2 = 0
    let hash = Hashtbl.hash
    let pp ppf (addr, port) =
      match Ipaddr.v4_of_v6 addr with
      | Some addr ->
          Format.fprintf ppf "%a:%d" Ipaddr.V4.pp_hum addr port
      | None ->
          Format.fprintf ppf "[%a]:%d" Ipaddr.V6.pp_hum addr port
    let pp_opt ppf = function
      | None -> Format.pp_print_string ppf "none"
      | Some point -> pp ppf point

    let is_local (addr, _) = Ipaddr.V6.is_private addr
    let is_global (addr, _) = not @@ Ipaddr.V6.is_private addr

    let of_string str =
      match String.rindex str ':' with
      | exception Not_found -> `Error "not a valid node address (ip:port)"
      | pos ->
          let len = String.length str in
          let addr, port =
            String.sub str 0 pos, String.sub str (pos+1) (len - pos - 1) in
          let addr = if addr = "" || addr = "_" then "[::]" else addr in
          match Ipaddr.of_string_exn addr, int_of_string port with
          | exception Failure _ -> `Error "not a valid node address (ip:port)"
          | V4 ipv4, port -> `Ok (Ipaddr.v6_of_v4 ipv4, port)
          | V6 ipv6, port -> `Ok (ipv6, port)

    let of_string_exn str =
      match of_string str with
      | `Ok saddr -> saddr
      | `Error msg -> invalid_arg msg

    let to_string saddr = Format.asprintf "%a" pp saddr

    let encoding =
      Data_encoding.conv to_string of_string_exn Data_encoding.string

  end

  include T

  (* Run-time point-or-gid indexed storage, one point is bound to at
     most one gid, which is the invariant we want to keep both for the
     connected peers table and the known peers one *)

  module Map = Map.Make (T)
  module Set = Set.Make (T)
  module Table = Hashtbl.Make (T)

end

module Id_point = struct

  module T = struct

    (* A net point (address x port). *)
    type t = addr * port option
    let empty = Ipaddr.V6.unspecified, None
    let compare (a1, p1) (a2, p2) =
      match Ipaddr.V6.compare a1 a2 with
      | 0 -> Pervasives.compare p1 p2
      | x -> x
    let equal p1 p2 = compare p1 p2 = 0
    let hash = Hashtbl.hash
    let pp ppf (addr, port) =
      match port with
      | None ->
          Format.fprintf ppf "[%a]:??" Ipaddr.V6.pp_hum addr
      | Some port ->
          Format.fprintf ppf "[%a]:%d" Ipaddr.V6.pp_hum addr port
    let pp_opt ppf = function
      | None -> Format.pp_print_string ppf "none"
      | Some point -> pp ppf point

    let is_local (addr, _) = Ipaddr.V6.is_private addr
    let is_global (addr, _) = not @@ Ipaddr.V6.is_private addr

    let encoding =
      let open Data_encoding in
      conv
        (fun (addr, port) -> Ipaddr.V6.to_bytes addr, port)
        (fun (addr, port) -> Ipaddr.V6.of_bytes_exn addr, port)
        (obj2
           (req "addr" string)
           (opt "port" int16))

  end

  include T

  (* Run-time point-or-gid indexed storage, one point is bound to at
     most one gid, which is the invariant we want to keep both for the
     connected peers table and the known peers one *)

  module Map = Map.Make (T)
  module Set = Set.Make (T)
  module Table = Hashtbl.Make (T)

end

module Identity = struct

  type t = {
    gid : Gid.t ;
    public_key : Crypto_box.public_key ;
    secret_key : Crypto_box.secret_key ;
    proof_of_work_stamp : Crypto_box.nonce ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { public_key ; secret_key ; proof_of_work_stamp } ->
         (public_key, secret_key, proof_of_work_stamp))
      (fun (public_key, secret_key, proof_of_work_stamp) ->
         let gid = Crypto_box.hash public_key in
         { gid ; public_key ; secret_key ; proof_of_work_stamp })
      (obj3
         (req "public_key" Crypto_box.public_key_encoding)
         (req "secret_key" Crypto_box.secret_key_encoding)
         (req "proof_of_work_stamp" Crypto_box.nonce_encoding))

  let generate ?max target =
    let secret_key, public_key, gid = Crypto_box.random_keypair () in
    let proof_of_work_stamp =
      Crypto_box.generate_proof_of_work ?max public_key target in
    { gid ; public_key ; secret_key ; proof_of_work_stamp }

  let animation = [|
    "|.....|" ;
    "|o....|" ;
    "|oo...|" ;
    "|ooo..|" ;
    "|.ooo.|" ;
    "|..ooo|" ;
    "|...oo|" ;
    "|....o|" ;
    "|.....|" ;
    "|.....|" ;
    "|.....|" ;
    "|.....|" ;
  |]

  let init = String.make (String.length animation.(0)) '\ '
  let clean = String.make (String.length animation.(0)) '\b'
  let animation = Array.map (fun x -> clean ^ x) animation
  let animation_size = Array.length animation
  let duration = 1200 / animation_size

  let generate_with_animation ppf target =
    Format.fprintf ppf "%s%!" init ;
    let count = ref 10000 in
    let rec loop n =
      let start = Mtime.counter () in
      Format.fprintf ppf "%s%!" animation.(n mod animation_size);
      try generate ~max:!count target
      with Not_found ->
        let time = Mtime.to_ms (Mtime.count start) in
        count :=
          if time <= 0. then
            !count * 10
          else
            !count * duration / int_of_float time ;
        loop (n+1)
    in
    let id = loop 0 in
    Format.fprintf ppf "%s%s\n%!" clean init ;
    id

  let generate target = generate target

end

module Connection_info = struct

  type t = {
    incoming : bool;
    gid : Gid.t;
    id_point : Id_point.t;
    remote_socket_port : port;
    versions : Version.t list ;
  }

  let pp ppf
      { incoming ; id_point = (remote_addr, remote_port) ; gid } =
    Format.fprintf ppf "%a:%a {%a}%s"
      Ipaddr.V6.pp_hum remote_addr
      (fun ppf port ->
         match port with
         | None -> Format.pp_print_string ppf "??"
         | Some port -> Format.pp_print_int ppf port) remote_port
      Gid.pp gid
      (if incoming then " (incoming)" else "")

end
