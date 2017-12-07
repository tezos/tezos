(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Resto_cohttp.Media_type.Make(RPC.Data).t = {
  name: Cohttp.Accept.media_range ;
  q: int option ;
  construct: 'a. 'a Data_encoding.t -> 'a -> string ;
  destruct: 'a. 'a Data_encoding.t -> string -> ('a, string) result ;
}

val json : t
val octet_stream : t

val all_media_types : t list


val accept_header : t list -> string
val first_complete_media : t list -> ((string * string) * t) option
