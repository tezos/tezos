(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Bigstring

include EndianBigstring.BigEndian
module LE = EndianBigstring.LittleEndian

include Compare.Make(struct
    type nonrec t = t
    let compare = Pervasives.compare
  end)

let make sz c =
  let buf = create sz in
  fill buf c ;
  buf

let to_hex t =
  Hex.of_cstruct (Cstruct.of_bigarray t)

let of_hex hex =
  Cstruct.to_bigarray (Hex.to_cstruct hex)
