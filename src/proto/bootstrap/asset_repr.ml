(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = int32
type asset = t
let of_int32 i = i
let to_int32 i = i

let encoding =
  let open Data_encoding in
  describe
    ~title: "Asset type"
    ~description: "A type of asset"
    (conv to_int32 of_int32 int32)


module Map = struct
  module Raw = Map.Make(struct
      type t = asset * Ed25519.Public_key_hash.t
      let compare (a1, pk1) (a2, pk2) =
        if Compare.Int32.(a1 = a2) then
          Ed25519.Public_key_hash.compare pk1 pk2
        else
          Compare.Int32.compare a1 a2
    end)
  type t = Tez_repr.tez Raw.t
  let empty = Raw.empty
  let add map asset key quantity =
    let previous_quantity =
      try Raw.find (asset, key) map
      with Not_found -> Tez_repr.zero in
    Tez_repr.(previous_quantity +? quantity) >>? fun total ->
    ok (Raw.add (asset, key) total map)

  let of_tuple_list_exn tl =
  List.fold_left
    (fun map (key, qty) -> Raw.add key qty map)
    Raw.empty tl


let encoding =
  let open Data_encoding in
  describe
    ~title: "Assets"
    ~description: "A list of assets held in the contract"
    (conv
       Raw.bindings
       (Json.wrap_error of_tuple_list_exn)
       (list
          (tup2
             (tup2 encoding Ed25519.Public_key_hash.encoding)
             Tez_repr.encoding)))

end

