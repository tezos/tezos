(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Unkwnon_alias_key of string

let () =
  register_error_kind `Permanent
    ~id: "signer.unknown_alias_key"
    ~title: "Unkwnon_alias_key"
    ~description: "A remote key does not exists"
    ~pp: (fun ppf s ->
        Format.fprintf ppf "The key %s does not is not known on the remote signer" s)
    Data_encoding.(obj1 (req "value" string))
    (function Unkwnon_alias_key s -> Some s | _ -> None)
    (fun s -> Unkwnon_alias_key s)

type key = string

module Sign = struct
  module Request = struct
    type t = {
      key : key ;
      data: MBytes.t ;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun { key ; data } ->
           ( key, data))
        (fun (key, data)  ->
           { key ; data })
        (obj2
           (req "key" string)
           (req "data" bytes))
  end

  module Response = struct
    type t = {
      signature : Signature.t
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun { signature } -> (signature))
        (fun (signature)  -> { signature })
        (obj1 (req "signature" Signature.encoding))
  end
end

module Public_key = struct
  module Request = struct
    type t = {
      key : key ;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun { key } -> key)
        (fun key  -> { key })
        (obj1 (req "key" string))
  end

  module Response = struct
    type t = {
      public_key : Signature.Public_key.t ;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun { public_key } -> public_key)
        (fun public_key  -> { public_key })
        (obj1 (req "pubkey" Signature.Public_key.encoding))
  end
end

module Request = struct
  type t =
    | Sign of Sign.Request.t
    | Public_key of Public_key.Request.t

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0) (merge_objs (obj1 (req "kind" (constant "sign"))) Sign.Request.encoding)
          (function Sign req -> Some ((), req) | _ -> None)
          (fun ((), req) -> Sign req) ;
        case (Tag 1) (merge_objs (obj1 (req "kind" (constant "public_key"))) Public_key.Request.encoding)
          (function Public_key req -> Some ((), req) | _ -> None)
          (fun ((), req) -> Public_key req) ]
end
