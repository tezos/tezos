module Block = struct
  type command =
    (* Activate a protocol *)
    | Activate

    (* Activate a protocol as a testnet *)
    | Activate_testnet

  type t = {
    command : command ;
    hash : Protocol_hash.t ;
    fitness : Int64.t ;
  }

  let mk_encoding name =
    let open Data_encoding in
    conv (fun (x, y) -> (), x, y) (fun ((), x, y) -> x, y)
      (obj3
         (req "network" (constant name))
         (req "hash" Protocol_hash.encoding)
         (req "fitness" int64))

  let encoding =
    let open Data_encoding in
    union ~tag_size:`Uint8 [
      case ~tag:0 (mk_encoding "main")
        (function { command = Activate ; hash ; fitness } ->
           Some (hash, fitness) | _ -> None)
        (fun (hash, fitness) -> { command = Activate ; hash ; fitness })
      ;
      case ~tag:1 (mk_encoding "test")
        (function { command = Activate_testnet ; hash ; fitness } ->
           Some (hash, fitness) | _ -> None)
        (fun (hash, fitness) -> { command = Activate_testnet ; hash ; fitness })
      ;
    ]

  let signed_encoding =
    let open Data_encoding in
    obj2
      (req "content" encoding)
      (req "signature" Ed25519.signature_encoding)
end

module Fitness = struct
  let fitness_key = ["v1";"store";"fitness"]

  let get_fitness ctxt =
    Context.get ctxt fitness_key >>= function
    | None -> Lwt.return 0L
    | Some b ->
        match Data_encoding.Binary.of_bytes Data_encoding.int64 b with
        | None -> Lwt.return 0L
        | Some v -> Lwt.return v

  let set_fitness ctxt v =
    Context.set ctxt fitness_key @@
    Data_encoding.Binary.to_bytes Data_encoding.int64 v

  let int64_to_bytes i =
    let b = MBytes.create 8 in
    MBytes.set_int64 b 0 i;
    b

  let header_fitness v =
    [ MBytes.of_string "\000" ; int64_to_bytes v ]
end
