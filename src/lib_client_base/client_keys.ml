(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Unregistered_key_scheme of string
type error += Invalid_uri of Uri.t

let () =
  register_error_kind `Permanent
    ~id: "cli.unregistered_key_scheme"
    ~title: "Unregistered key scheme"
    ~description: "A key has been provided with an \
                   unregistered scheme (no corresponding plugin)"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "No matching plugin for key scheme %s" s)
    Data_encoding.(obj1 (req "value" string))
    (function Unregistered_key_scheme s -> Some s | _ -> None)
    (fun s -> Unregistered_key_scheme s) ;
  register_error_kind `Permanent
    ~id: "cli.key.invalid_uri"
    ~title: "Invalid key uri"
    ~description: "A key has been provided with an invalid uri."
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "Cannot parse the key uri: %s" s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_uri s -> Some (Uri.to_string s) | _ -> None)
    (fun s -> Invalid_uri (Uri.of_string s))

module Public_key_hash = Client_aliases.Alias (struct
    type t =  Signature.Public_key_hash.t
    let encoding = Signature.Public_key_hash.encoding
    let of_source s = Lwt.return (Signature.Public_key_hash.of_b58check s)
    let to_source p = return (Signature.Public_key_hash.to_b58check p)
    let name = "public key hash"
  end)

module type KEY = sig
  type t
  val to_b58check : t -> string
  val of_b58check_exn : string -> t
end

let uri_encoding =
  Data_encoding.(conv Uri.to_string Uri.of_string string)

type pk_uri = Uri.t
let make_pk_uri x = x

type sk_uri = Uri.t
let make_sk_uri x = x

let pk_uri_param ?name ?desc params =
  let name = Option.unopt ~default:"uri" name in
  let desc = Option.unopt
      ~default:"public key\n\
                Varies from one scheme to the other.\n\
                Use command `list signing schemes` for more \
                information." desc in
  let open Clic in
  param ~name ~desc (parameter (fun _ s ->
      try return (make_pk_uri @@ Uri.of_string s)
      with Failure s -> failwith "Error while parsing uri: %s" s))
    params

let sk_uri_param ?name ?desc params =
  let name = Option.unopt ~default:"uri" name in
  let desc = Option.unopt
      ~default:"secret key\n\
                Varies from one scheme to the other.\n\
                Use command `list signing schemes` for more \
                information." desc in
  let open Clic in
  param ~name ~desc
    (parameter (fun _ s ->
         try return (make_sk_uri @@ Uri.of_string s)
         with Failure s -> failwith "Error while parsing uri: %s" s))
    params

module Secret_key =
  Client_aliases.Alias (struct
    let name = "secret_key"
    type t = Uri.t
    let pp = Uri.pp_hum
    let of_source s = return (Uri.of_string s)
    let to_source t = return (Uri.to_string t)
    let encoding = uri_encoding
  end)

module Public_key =
  Client_aliases.Alias (struct
    let name = "public_key"
    type t = Uri.t * Signature.Public_key.t option
    let pp ppf (loc, _) = Uri.pp_hum ppf loc
    let of_source s = return (Uri.of_string s, None)
    let to_source (t, _) = return (Uri.to_string t)
    let encoding =
      let open Data_encoding in
      union
        [ case Json_only
            ~title: "Locator_only"
            uri_encoding
            (function (uri, None) -> Some uri | (_, Some _) -> None)
            (fun uri -> (uri, None)) ;
          case Json_only
            ~title: "Locator_and_full_key"
            (obj2
               (req "locator" uri_encoding)
               (req "key" Signature.Public_key.encoding))
            (function (uri, Some key) -> Some (uri, key) | (_, None) -> None)
            (fun (uri, key) -> (uri, Some key)) ]
  end)

module type SIGNER = sig
  val scheme : string
  val title : string
  val description : string
  val neuterize : sk_uri -> pk_uri tzresult Lwt.t
  val public_key : pk_uri -> Signature.Public_key.t tzresult Lwt.t
  val public_key_hash : pk_uri -> (Signature.Public_key_hash.t * Signature.Public_key.t option) tzresult Lwt.t
  val sign :
    ?watermark: Signature.watermark ->
    sk_uri -> MBytes.t -> Signature.t tzresult Lwt.t
end

let signers_table : (string, (module SIGNER)) Hashtbl.t = Hashtbl.create 13

let register_signer signer =
  let module Signer = (val signer : SIGNER) in
  Hashtbl.replace signers_table Signer.scheme signer

let find_signer_for_key ~scheme =
  match Hashtbl.find signers_table scheme with
  | exception Not_found ->
      fail (Unregistered_key_scheme scheme)
  | signer -> return signer

let registered_signers () : (string * (module SIGNER)) list =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) signers_table []

type error += Signature_mismatch of sk_uri

let () =
  register_error_kind `Permanent
    ~id: "cli.signature_mismatch"
    ~title: "Signature mismatch"
    ~description: "The signer produced an invalid signature"
    ~pp:
      (fun ppf sk ->
         Format.fprintf ppf
           "The signer for %a produced an invalid signature"
           Uri.pp_hum sk)
    Data_encoding.(obj1 (req "locator" uri_encoding))
    (function Signature_mismatch sk -> Some sk | _ -> None)
    (fun sk -> Signature_mismatch sk)

let neuterize sk_uri =
  let scheme = Option.unopt ~default:"" (Uri.scheme sk_uri) in
  find_signer_for_key ~scheme >>=? fun signer ->
  let module Signer = (val signer : SIGNER) in
  Signer.neuterize sk_uri

let public_key pk_uri =
  let scheme = Option.unopt ~default:"" (Uri.scheme pk_uri) in
  find_signer_for_key ~scheme >>=? fun signer ->
  let module Signer = (val signer : SIGNER) in
  Signer.public_key pk_uri

let public_key_hash pk_uri =
  public_key pk_uri >>=? fun pk ->
  return (Signature.Public_key.hash pk, Some pk)

let sign ?watermark sk_uri buf =
  let scheme = Option.unopt ~default:"" (Uri.scheme sk_uri) in
  find_signer_for_key ~scheme >>=? fun signer ->
  let module Signer = (val signer : SIGNER) in
  Signer.sign ?watermark sk_uri buf >>=? fun signature ->
  Signer.neuterize sk_uri >>=? fun pk_uri ->
  public_key pk_uri >>=? fun pubkey ->
  fail_unless
    (Signature.check ?watermark pubkey signature buf)
    (Signature_mismatch sk_uri) >>=? fun () ->
  return signature

let append ?watermark loc buf =
  sign ?watermark loc buf >>|? fun signature ->
  Signature.concat buf signature

let check ?watermark pk_uri signature buf =
  public_key pk_uri >>=? fun pk ->
  return (Signature.check ?watermark pk signature buf)

let register_key cctxt ?(force=false) (public_key_hash, pk_uri, sk_uri) ?public_key name =
  Public_key.add ~force cctxt name (pk_uri, public_key) >>=? fun () ->
  Secret_key.add ~force cctxt name sk_uri >>=? fun () ->
  Public_key_hash.add ~force cctxt name public_key_hash >>=? fun () ->
  return ()

let raw_get_key (cctxt : #Client_context.wallet) pkh =
  begin
    Public_key_hash.rev_find cctxt pkh >>=? function
    | None -> failwith "no keys for the source contract manager"
    | Some n ->
        Secret_key.find_opt cctxt n >>=? fun sk_uri ->
        Public_key.find_opt cctxt n >>=? begin function
          | None -> return None
          | Some (_, Some pk) -> return (Some pk)
          | Some (pk_uri, None) ->
              public_key pk_uri >>=? fun pk ->
              Public_key.update cctxt n (pk_uri, Some pk) >>=? fun () ->
              return (Some pk)
        end >>=? fun pk ->
        return (n, pk, sk_uri)
  end >>= function
  | (Ok (_, None, None) | Error _) as initial_result -> begin
      begin
        (* try to lookup for a remote key *)
        find_signer_for_key ~scheme:"remote" >>=? fun signer ->
        let module Signer = (val signer : SIGNER) in
        let path = Signature.Public_key_hash.to_b58check pkh in
        let uri = Uri.make ~scheme:Signer.scheme ~path () in
        Signer.public_key uri >>=? fun pk ->
        return (path, Some pk, Some uri)
      end >>= function
      | Error _ -> Lwt.return initial_result
      | Ok _ as success -> Lwt.return success
    end
  | Ok _ as success -> Lwt.return success

let get_key cctxt pkh =
  raw_get_key cctxt pkh >>=? function
  | (pkh, Some pk, Some sk) -> return (pkh, pk, sk)
  | (_pkh, _pk, None) -> failwith "Unknown secret key for %a" Signature.Public_key_hash.pp pkh
  | (_pkh, None, _sk) -> failwith "Unknown public key for %a" Signature.Public_key_hash.pp pkh

let get_public_key cctxt pkh =
  raw_get_key cctxt pkh >>=? function
  | (pkh, Some pk, _sk) -> return (pkh, pk)
  | (_pkh, None, _sk) -> failwith "Unknown public key for %a" Signature.Public_key_hash.pp pkh

let get_keys (cctxt : #Client_context.wallet) =
  Secret_key.load cctxt >>=? fun sks ->
  Lwt_list.filter_map_s begin fun (name, sk_uri) ->
    begin
      Public_key_hash.find cctxt name >>=? fun pkh ->
      Public_key.find cctxt name >>=? begin function
        | _, Some pk -> return pk
        | pk_uri, None ->
            public_key pk_uri >>=? fun pk ->
            Public_key.update cctxt name (pk_uri, Some pk) >>=? fun () ->
            return pk
      end >>=? fun pk ->
      return (name, pkh, pk, sk_uri)
    end >>= function
    | Ok r -> Lwt.return (Some r)
    | Error _ -> Lwt.return_none
  end sks >>= fun keys ->
  return keys

let list_keys cctxt =
  Public_key_hash.load cctxt >>=? fun l ->
  map_s
    (fun (name, pkh) ->
       raw_get_key cctxt pkh >>= function
       | Ok (_name, pk, sk_uri) ->
           return (name, pkh, pk, sk_uri)
       | Error _ ->
           return (name, pkh, None, None))
    l

let alias_keys cctxt name =
  Public_key_hash.find cctxt name >>=? fun pkh ->
  raw_get_key cctxt pkh >>= function
  | Ok (_name, pk, sk_uri) -> return (Some (pkh, pk, sk_uri))
  | Error _ -> return None

let force_switch () =
  Clic.switch
    ~long:"force" ~short:'f'
    ~doc:"overwrite existing keys" ()
