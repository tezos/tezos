(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let generate len =
  Cstruct.to_bigarray (Tweetnacl.Rand.gen len)

let generate_into ?(pos=0) ?len buf =
  let buflen = MBytes.length buf in
  let len = match len with
    | Some len -> len
    | None -> buflen - pos in
  if pos < 0 || len < 0 || pos + len > buflen then
    invalid_arg (Printf.sprintf "Rand.generate_into: \
                                 invalid slice (pos=%d len=%d)" pos len) ;
  let cs = Cstruct.of_bigarray buf in
  let cs = Cstruct.sub cs pos len in
  Tweetnacl.Rand.write cs
