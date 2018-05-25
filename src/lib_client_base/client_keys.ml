(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Unregistered_key_scheme of string
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
    (fun s -> Unregistered_key_scheme s)

module Public_key_hash = Client_aliases.Alias (struct
    type t =  Signature.Public_key_hash.t
    let encoding = Signature.Public_key_hash.encoding
    let of_source s = Lwt.return (Signature.Public_key_hash.of_b58check s)
    let to_source p = return (Signature.Public_key_hash.to_b58check p)
    let name = "public key hash"
  end)

module type LOCATOR = sig
  val name : string
  type t

  val create : scheme:string -> location:string -> t
  val scheme : t -> string
  val location : t -> string
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

type sk_locator = Sk_locator of { scheme : string ; location : string }
type pk_locator = Pk_locator of { scheme : string ; location : string }

module Sk_locator = struct
  let name = "secret key"
  type t = sk_locator

  let create ~scheme ~location =
    Sk_locator { scheme ; location }

  let scheme (Sk_locator { scheme }) = scheme
  let location (Sk_locator { location }) = location

  let to_string (Sk_locator { scheme ; location }) =
    scheme ^ ":" ^  location

  let pp ppf (Sk_locator { scheme ; location }) =
    Format.pp_print_string ppf (scheme ^ ":" ^ location)
end

module Pk_locator = struct
  let name = "public key"
  type t = pk_locator

  let create ~scheme ~location =
    Pk_locator { scheme ; location }

  let scheme (Pk_locator { scheme }) = scheme
  let location (Pk_locator { location }) = location

  let to_string (Pk_locator { scheme ; location }) =
    scheme ^ ":" ^  location

  let pp ppf (Pk_locator { scheme ; location }) =
    Format.pp_print_string ppf (scheme ^ ":" ^ location)
end

module type KEY = sig
  type t
  val to_b58check : t -> string
  val of_b58check_exn : string -> t
end

module Locator (K : KEY) (L : LOCATOR) = struct
  include L

  let of_unencrypted k =
    L.create ~scheme:"unencrypted"
      ~location:(K.to_b58check k)

  let of_string s =
    match String.index s ':' with
    | exception Not_found ->
        of_unencrypted (K.of_b58check_exn s)
    | i ->
        let len = String.length s in
        create
          ~scheme:(String.sub s 0 i)
          ~location:(String.sub s (i+1) (len-i-1))

  let of_source s =  return (of_string s)
  let to_source t = return (to_string t)

  let encoding = Data_encoding.(conv to_string of_string string)
end

module Secret_key_locator = Locator(Signature.Secret_key)(Sk_locator)
module Secret_key = Client_aliases.Alias (Secret_key_locator)
module Public_key_locator = Locator(Signature.Public_key)(Pk_locator)
module Public_key = Client_aliases.Alias (Public_key_locator)

module type SIGNER = sig
  type secret_key
  type public_key
  val scheme : string
  val title : string
  val description : string
  val init : #Client_context.io_wallet -> unit tzresult Lwt.t
  val sk_locator_of_human_input : #Client_context.io_wallet -> string list -> sk_locator tzresult Lwt.t
  val pk_locator_of_human_input : #Client_context.io_wallet -> string list -> pk_locator tzresult Lwt.t
  val sk_of_locator : sk_locator -> secret_key tzresult Lwt.t
  val pk_of_locator : pk_locator -> public_key tzresult Lwt.t
  val sk_to_locator : secret_key -> sk_locator Lwt.t
  val pk_to_locator : public_key -> pk_locator Lwt.t
  val neuterize : secret_key -> public_key Lwt.t
  val public_key : public_key -> Signature.Public_key.t Lwt.t
  val public_key_hash : public_key -> Signature.Public_key_hash.t Lwt.t
  val sign :
    ?watermark: Signature.watermark ->
    secret_key -> MBytes.t -> Signature.t tzresult Lwt.t
end

let signers_table : (string, (module SIGNER) * bool) Hashtbl.t = Hashtbl.create 13

let register_signer signer =
  let module Signer = (val signer : SIGNER) in
  Hashtbl.replace signers_table Signer.scheme (signer, false)

let find_signer_for_key cctxt ~scheme =
  match Hashtbl.find signers_table scheme with
  | exception Not_found -> fail (Unregistered_key_scheme scheme)
  | signer, false ->
      let module Signer = (val signer : SIGNER) in
      Signer.init cctxt >>=? fun () ->
      Hashtbl.replace signers_table scheme (signer, true) ;
      return signer
  | signer, true -> return signer

let registered_signers () : (string * (module SIGNER)) list =
  Hashtbl.fold (fun k (v, _) acc -> (k, v) :: acc) signers_table []

let sign ?watermark cctxt ((Sk_locator { scheme }) as skloc) buf =
  find_signer_for_key cctxt ~scheme >>=? fun signer ->
  let module Signer = (val signer : SIGNER) in
  Signer.sk_of_locator skloc >>=? fun t ->
  Signer.sign ?watermark t buf

let append ?watermark cctxt loc buf =
  sign ?watermark cctxt loc buf >>|? fun signature ->
  Signature.concat buf signature

let register_key cctxt ?(force=false)
    (public_key_hash, public_key, secret_key) name =
  Secret_key.add ~force cctxt name
    (Secret_key_locator.of_unencrypted secret_key) >>=? fun () ->
  Public_key.add ~force cctxt name
    (Public_key_locator.of_unencrypted public_key) >>=? fun () ->
  Public_key_hash.add ~force
    cctxt name public_key_hash >>=? fun () ->
  return ()

let gen_keys ?(force=false) ?algo ?seed (cctxt : #Client_context.io_wallet) name =
  let key = Signature.generate_key ?algo ?seed () in
  register_key cctxt ~force key name

let gen_keys_containing ?(prefix=false) ?(force=false) ~containing ~name (cctxt : #Client_context.full) =
  let unrepresentable =
    List.filter (fun s -> not @@ Base58.Alphabet.all_in_alphabet Base58.Alphabet.bitcoin s) containing in
  match unrepresentable with
  | _ :: _ ->
      cctxt#warning
        "The following can't be written in the key alphabet (%a): %a"
        Base58.Alphabet.pp Base58.Alphabet.bitcoin
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           (fun ppf s -> Format.fprintf ppf "'%s'" s))
        unrepresentable >>= return
  | [] ->
      Public_key_hash.mem cctxt name >>=? fun name_exists ->
      if name_exists && not force
      then
        cctxt#warning
          "Key for name '%s' already exists. Use -force to update." name >>= return
      else
        begin
          cctxt#warning "This process uses a brute force search and \
                         may take a long time to find a key." >>= fun () ->
          let matches =
            if prefix then
              let containing_tz1 = List.map ((^) "tz1") containing in
              (fun key -> List.exists
                  (fun containing ->
                     String.sub key 0 (String.length containing) = containing)
                  containing_tz1)
            else
              let re = Re.Str.regexp (String.concat "\\|" containing) in
              (fun key -> try ignore (Re.Str.search_forward re key 0); true
                with Not_found -> false) in
          let rec loop attempts =
            let public_key_hash, public_key, secret_key =
              Signature.generate_key () in
            let hash = Signature.Public_key_hash.to_b58check @@
              Signature.Public_key.hash public_key in
            if matches hash
            then
              Secret_key.add ~force cctxt name
                (Secret_key_locator.of_unencrypted secret_key) >>=? fun () ->
              Public_key.add ~force cctxt name
                (Public_key_locator.of_unencrypted public_key) >>=? fun () ->
              Public_key_hash.add ~force cctxt name public_key_hash >>=? fun () ->
              return hash
            else begin if attempts mod 25_000 = 0
              then cctxt#message "Tried %d keys without finding a match" attempts
              else Lwt.return () end >>= fun () ->
              loop (attempts + 1) in
          loop 1 >>=? fun key_hash ->
          cctxt#message
            "Generated '%s' under the name '%s'." key_hash name >>= fun () ->
          return ()
        end

let get_key (cctxt : #Client_context.wallet) pkh =
  Public_key_hash.rev_find cctxt pkh >>=? function
  | None -> failwith "no keys for the source contract manager"
  | Some n ->
      Public_key.find cctxt n >>=? fun pk ->
      Secret_key.find cctxt n >>=? fun sk ->
      let scheme = Secret_key_locator.scheme sk in
      find_signer_for_key cctxt ~scheme >>=? fun signer ->
      let module Signer = (val signer : SIGNER) in
      Signer.pk_of_locator pk >>=? fun pk ->
      Signer.public_key pk >>= fun pk ->
      return (n, pk, sk)

let get_keys (wallet : #Client_context.io_wallet) =
  Secret_key.load wallet >>=? fun sks ->
  Lwt_list.filter_map_s begin fun (name, sk) ->
    begin
      Public_key.find wallet name >>=? fun pk ->
      Public_key_hash.find wallet name >>=? fun pkh ->
      let scheme = Public_key_locator.scheme pk in
      find_signer_for_key wallet ~scheme >>=? fun signer ->
      let module Signer = (val signer : SIGNER) in
      Signer.pk_of_locator pk >>=? fun pk ->
      Signer.public_key pk >>= fun pk ->
      return (name, pkh, pk, sk)
    end >>= function
    | Ok r -> Lwt.return (Some r)
    | Error _ -> Lwt.return_none
  end sks >>= fun keys ->
  return keys

let list_keys cctxt =
  Public_key_hash.load cctxt >>=? fun l ->
  map_s
    (fun (name, pkh) ->
       Public_key.find_opt cctxt name >>=? fun pkm ->
       Secret_key.find_opt cctxt name >>=? fun pks ->
       return (name, pkh, pkm, pks))
    l

let alias_keys cctxt name =
  Public_key_hash.load cctxt >>=? fun l ->
  let rec find_key = function
    | [] -> return None
    | (key_name, pkh) :: tl ->
        if key_name = name
        then
          Public_key.find_opt cctxt name >>=? fun pkm ->
          Secret_key.find_opt cctxt name >>=? fun pks ->
          return (Some (pkh, pkm, pks))
        else find_key tl
  in find_key l

let force_switch () =
  Clic.switch
    ~long:"force" ~short:'f'
    ~doc:"overwrite existing keys" ()
