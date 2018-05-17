(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Ipaddr.V6.t

let encoding =
  let open Data_encoding in
  splitted
    ~json:begin
      conv
        Ipaddr.V6.to_string
        Ipaddr.V6.of_string_exn
        string
    end
    ~binary:begin
      conv
        Ipaddr.V6.to_bytes
        Ipaddr.V6.of_bytes_exn
        string
    end

type port = int

let pp ppf addr =
  match Ipaddr.v4_of_v6 addr with
  | Some addr ->
      Format.fprintf ppf "%a" Ipaddr.V4.pp_hum addr
  | None ->
      Format.fprintf ppf "[%a]" Ipaddr.V6.pp_hum addr

let of_string_opt str =
  Option.map (Ipaddr.of_string str) ~f:begin function
    | Ipaddr.V4 addr -> Ipaddr.v6_of_v4 addr
    | V6 addr -> addr
  end

let of_string_exn str =
  match of_string_opt str with
  | None -> Pervasives.failwith "P2p_addr.of_string"
  | Some t -> t

let to_string saddr = Format.asprintf "%a" pp saddr
