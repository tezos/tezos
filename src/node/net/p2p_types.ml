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
