(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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
