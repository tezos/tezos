(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(H : sig
    type t
    val title: string
    val name: string
    val b58check_encoding: t Base58.encoding
    val raw_encoding: t Data_encoding.t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val hash: t -> int
  end) = struct

  let of_b58check_opt s =
    Base58.simple_decode H.b58check_encoding s
  let of_b58check_exn s =
    match Base58.simple_decode H.b58check_encoding s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected hash (%s)" H.name
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        Error_monad.generic_error "Failed to read a base58-encoded hash (%s)" H.name
  let to_b58check s = Base58.simple_encode H.b58check_encoding s
  let to_short_b58check s =
    String.sub
      (to_b58check s) 0
      (10 + String.length (Base58.prefix H.b58check_encoding))

  let pp ppf t =
    Format.pp_print_string ppf (to_b58check t)

  let pp_short ppf t =
    Format.pp_print_string ppf (to_short_b58check t)

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        H.raw_encoding
      ~json:
        (describe ~title: (H.title ^ " (Base58Check-encoded Blake2B hash)") @@
         conv to_b58check (Data_encoding.Json.wrap_error of_b58check_exn) string)

  let rpc_arg =
    RPC_arg.make
      ~name:(Format.asprintf "hash.%s" H.name)
      ~descr:(Format.asprintf "A b58check-encoded hash (%s)" H.name)
      ~destruct:
        (fun s ->
           match of_b58check_opt s with
           | None ->
               Error (Format.asprintf
                        "failed to decode b58check-encoded hash (%s): %S"
                        H.name s)
           | Some v -> Ok v)
      ~construct:to_b58check
      ()

  let param ?(name=H.name) ?(desc=H.title) t =
    Clic.param
      ~name
      ~desc (Clic.parameter (fun _ str -> Lwt.return (of_b58check str))) t

  module Set = struct
    include Set.Make(struct type t = H.t let compare = H.compare end)
    exception Found of elt
    let random_elt s =
      let n = Random.int (cardinal s) in
      try
        ignore
          (fold (fun x i -> if i = n then raise (Found x) ; i+1) s 0 : int) ;
        assert false
      with Found x -> x
    let encoding =
      Data_encoding.conv
        elements
        (fun l -> List.fold_left (fun m x -> add x m) empty l)
        Data_encoding.(list encoding)
  end

  module Table = struct
    include Hashtbl.Make(struct
        type t = H.t
        let hash = H.hash
        let equal = H.equal
      end)
    let encoding arg_encoding =
      Data_encoding.conv
        (fun h -> fold (fun k v l -> (k, v) :: l) h [])
        (fun l ->
           let h = create (List.length l) in
           List.iter (fun (k,v) -> add h k v) l ;
           h)
        Data_encoding.(list (tup2 encoding arg_encoding))
  end

  module Map = struct
    include Map.Make(struct type t = H.t let compare = H.compare end)
    let encoding arg_encoding =
      Data_encoding.conv
        bindings
        (fun l -> List.fold_left (fun m (k,v) -> add k v m) empty l)
        Data_encoding.(list (tup2 encoding arg_encoding))
  end

end
