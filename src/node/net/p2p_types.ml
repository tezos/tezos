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

  let pp ppf { name ; major ; minor } =
    Format.fprintf ppf "%s.%d.%d" name major minor

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
    total_sent : int64 ;
    total_recv : int64 ;
    current_inflow : int ;
    current_outflow : int ;
  }

  let empty = {
    total_sent = 0L ;
    total_recv = 0L ;
    current_inflow = 0 ;
    current_outflow = 0 ;
  }

  let print_size ppf sz =
    let ratio n = (float_of_int sz /. float_of_int (1 lsl n)) in
    if sz < 1 lsl 10 then
      Format.fprintf ppf "%d B" sz
    else if sz < 1 lsl 20 then
      Format.fprintf ppf "%.2f kiB" (ratio 10)
    else
      Format.fprintf ppf "%.2f MiB" (ratio 20)

  let print_size64 ppf sz =
    let open Int64 in
    let ratio n = (to_float sz /. float_of_int (1 lsl n)) in
    if sz < shift_left 1L 10 then
      Format.fprintf ppf "%Ld B" sz
    else if sz < shift_left 1L 20 then
      Format.fprintf ppf "%.2f kiB" (ratio 10)
    else if sz < shift_left 1L 30 then
      Format.fprintf ppf "%.2f MiB" (ratio 20)
    else if sz < shift_left 1L 40 then
      Format.fprintf ppf "%.2f GiB" (ratio 30)
    else
      Format.fprintf ppf "%.2f TiB" (ratio 40)

  let pp ppf stat =
    Format.fprintf ppf
      "↗ %a (%a/s) ↘ %a (%a/s)"
      print_size64 stat.total_sent print_size stat.current_outflow
      print_size64 stat.total_recv print_size stat.current_inflow

  let encoding =
    let open Data_encoding in
    conv
      (fun { total_sent ; total_recv ; current_inflow ; current_outflow } ->
         (total_sent, total_recv, current_inflow, current_outflow))
      (fun (total_sent, total_recv, current_inflow, current_outflow) ->
         { total_sent ; total_recv ; current_inflow ; current_outflow })
      (obj4
         (req "total_sent" int64)
         (req "total_recv" int64)
         (req "current_inflow" int31)
         (req "current_outflow" int31))
end

module Peer_id = Crypto_box.Public_key_hash

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

    let of_string_exn str =
      let addr, port = Utils.parse_addr_port str in
      let port = int_of_string port in
      if port < 0 && port > 1 lsl 16 - 1 then
        invalid_arg "port must be between 0 and 65535" ;
      match Ipaddr.of_string_exn addr with
      | V4 addr -> Ipaddr.v6_of_v4 addr, port
      | V6 addr -> addr, port

    let of_string str =
      try Ok (of_string_exn str) with
      | Invalid_argument s -> Error s
      | Failure s -> Error s
      | _ -> Error "Point.of_string"

    let to_string saddr = Format.asprintf "%a" pp saddr

    let encoding =
      Data_encoding.conv to_string of_string_exn Data_encoding.string

  end

  include T

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
    let to_string t = Format.asprintf "%a" pp t

    let is_local (addr, _) = Ipaddr.V6.is_private addr
    let is_global (addr, _) = not @@ Ipaddr.V6.is_private addr

    let of_point (addr, port) = addr, Some port
    let to_point = function
      | _, None -> None
      | addr, Some port -> Some (addr, port)
    let to_point_exn = function
      | _, None -> invalid_arg "to_point_exn"
      | addr, Some port -> addr, port

    let encoding =
      let open Data_encoding in
      conv
        (fun (addr, port) -> Ipaddr.V6.to_string addr, port)
        (fun (addr, port) -> Ipaddr.V6.of_string_exn addr, port)
        (obj2
           (req "addr" string)
           (opt "port" uint16))

  end

  include T

  module Map = Map.Make (T)
  module Set = Set.Make (T)
  module Table = Hashtbl.Make (T)

end

module Identity = struct

  type t = {
    peer_id : Peer_id.t ;
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
         let peer_id = Crypto_box.hash public_key in
         { peer_id ; public_key ; secret_key ; proof_of_work_stamp })
      (obj3
         (req "public_key" Crypto_box.public_key_encoding)
         (req "secret_key" Crypto_box.secret_key_encoding)
         (req "proof_of_work_stamp" Crypto_box.nonce_encoding))

  let generate ?max target =
    let secret_key, public_key, peer_id = Crypto_box.random_keypair () in
    let proof_of_work_stamp =
      Crypto_box.generate_proof_of_work ?max public_key target in
    { peer_id ; public_key ; secret_key ; proof_of_work_stamp }

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
    peer_id : Peer_id.t;
    id_point : Id_point.t;
    remote_socket_port : port;
    versions : Version.t list ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { incoming ; peer_id ; id_point ; remote_socket_port ; versions } ->
         (incoming, peer_id, id_point, remote_socket_port, versions))
      (fun (incoming, peer_id, id_point, remote_socket_port, versions) ->
         { incoming ; peer_id ; id_point ; remote_socket_port ; versions })
      (obj5
         (req "incoming" bool)
         (req "peer_id" Peer_id.encoding)
         (req "id_point" Id_point.encoding)
         (req "remote_socket_port" uint16)
         (req "versions" (list Version.encoding)))

  let pp ppf
      { incoming ; id_point = (remote_addr, remote_port) ;
        remote_socket_port ; peer_id ; versions } =
    let version = List.hd versions in
    let point = match remote_port with
      | None -> remote_addr, remote_socket_port
      | Some port -> remote_addr, port in
    Format.fprintf ppf "%s %a %a (%a)"
      (if incoming then "↘" else "↗")
      Peer_id.pp peer_id
      Point.pp point
      Version.pp version
end
