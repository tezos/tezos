(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Client_keys

include Tezos_stdlib.Logging.Make(struct let name = "client.signer.ledger" end)

let scheme = "ledger"

let title =
  "Built-in signer using Ledger Nano S."

let description =
  "Valid URIs are of the form\n\
  \ - ledger://<root_pkh>[/<path>]\n\
   where <root_pkh> is the Base58-encoded public key hash of the key \
   at m/44'/1729' and <path> is a BIP32 path anchored at \
   m/44'/1729'. Ledger does not yet support non-hardened path so each \
   node of the path must be hardened.\n\
   Use `tezos-client list connected ledgers` to get the <root_pkh> of \
   all connected devices."

let hard = Int32.logor 0x8000_0000l
let unhard = Int32.logand 0x7fff_ffffl
let is_hard n = Int32.logand 0x8000_0000l n <> 0l
let tezos_root = [hard 44l ; hard 1729l]

module Bip32_path = struct
  let node_of_string str =
    match Int32.of_string_opt str with
    | Some node -> Some node
    | None ->
        match Int32.of_string_opt String.(sub str 0 ((length str) - 1)) with
        | None -> None
        | Some node -> Some (hard node)

  let node_of_string_exn str =
    match node_of_string str with
    | None ->
        invalid_arg (Printf.sprintf "node_of_string_exn: got %S" str)
    | Some str -> str

  let pp_node ppf node =
    match is_hard node with
    | true -> Fmt.pf ppf "%ld'" (unhard node)
    | false -> Fmt.pf ppf "%ld" node

  let string_of_node = Fmt.to_to_string pp_node

  let path_of_string_exn s =
    match String.split_on_char '/' s with
    | [""] -> []
    | nodes ->
        List.map node_of_string_exn nodes

  let path_of_string s =
    try Some (path_of_string_exn s) with _ -> None

  let pp_path =
    Fmt.(list ~sep:(const char '/') pp_node)

  let string_of_path = Fmt.to_to_string pp_path
end

(* Those are always valid on Ledger Nano S with latest firmware. *)
let vendor_id = 0x2c97
let product_id = 0x0001

let pks : (pk_uri, Signature.Public_key.t) Hashtbl.t =
  Hashtbl.create 13

let pkhs : (pk_uri, Signature.Public_key_hash.t) Hashtbl.t =
  Hashtbl.create 13

let curve_of_pkh :
  Signature.public_key_hash -> Ledgerwallet_tezos.curve = function
  | Ed25519 _ -> Ledgerwallet_tezos.Ed25519
  | Secp256k1 _ -> Secp256k1
  | P256 _ -> Secp256r1

let secp256k1_ctx =
  Libsecp256k1.External.Context.create ~sign:false ~verify:false ()

type error +=
  | LedgerError of Ledgerwallet.Transport.error
  | Ledger_deterministic_nonce_not_implemented

let error_encoding =
  let open Data_encoding in
  conv
    (fun e -> Format.asprintf "%a" Ledgerwallet.Transport.pp_error e)
    (fun _ ->invalid_arg "Ledger error is not deserializable")
    (obj1 (req "ledger-error" string))

let () =
  register_error_kind
    `Permanent
    ~id: "signer.ledger"
    ~title: "Ledger error"
    ~description: "Error when communication to a Ledger Nano S device"
    ~pp:(fun ppf e ->
        Format.fprintf ppf "Ledger %a" Ledgerwallet.Transport.pp_error e)
    error_encoding
    (function LedgerError e -> Some e | _ -> None)
    (fun e -> LedgerError e)

let () =
  register_error_kind
    `Permanent
    ~id: "signer.ledger.deterministic_nonce_not_implemented"
    ~title: "Ledger deterministic_nonce(_hash) not implemented"
    ~description: "The deterministic_nonce(_hash) functionality \
                   is not implemented by the ledger"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Asked the ledger to generate a  deterministic nonce (hash), \
                            but this functionality is not yet implemented")
    Data_encoding.unit
    (function Ledger_deterministic_nonce_not_implemented -> Some () | _ -> None)
    (fun () -> Ledger_deterministic_nonce_not_implemented)

type id =
  | Animals of Ledger_names.t * Ledgerwallet_tezos.curve option
  | Pkh of Signature.Public_key_hash.t

let pp_id ppf = function
  | Pkh pkh -> Signature.Public_key_hash.pp ppf pkh
  | Animals (cthd, curve) ->
      Format.fprintf ppf "%a%a" Ledger_names.pp cthd
        (fun fmt -> function
           | None -> Format.fprintf fmt ""
           | Some a -> Format.fprintf fmt "/%a" Ledgerwallet_tezos.pp_curve a)
        curve

let pp_animals_uri ppf (names, curve, path) =
  let (root, path_without_root) = List.split_n (List.length tezos_root) path in
  if root <> tezos_root then
    Format.kasprintf Pervasives.failwith "BIP32 path is missing Tezos BIP32 prefix of %a: %a" Bip32_path.pp_path tezos_root Bip32_path.pp_path path
  else
    Format.fprintf ppf "ledger://%a%a" pp_id (Animals (names, Some curve))
      (fun fmt -> function
         | [] -> Format.fprintf fmt ""
         | xs -> Format.fprintf fmt "/%a" Bip32_path.pp_path xs)
      path_without_root

let parse_animals animals =
  match String.split '-' animals with
  | [c; t; h; d] -> Some { Ledger_names.c ; t ; h ; d }
  | _ -> None

let id_of_uri uri =
  let host = Uri.host uri in
  match Option.apply host
          ~f:Signature.Public_key_hash.of_b58check_opt with
  | Some pkh -> return (Pkh pkh)
  | None ->
      match Option.apply host ~f:parse_animals,
            Option.apply (List.hd_opt (String.split '/' (Uri.path uri)))
              ~f:Ledgerwallet_tezos.curve_of_string with
      | Some animals, curve ->
          return (Animals (animals, curve))
      | (ann, curr) ->
          failwith "No public key hash or animal names in %a (%a, %a)"
            Uri.pp_hum uri
            (fun fmt -> function
               | None -> Format.fprintf fmt "NONE"
               | Some a -> Format.fprintf fmt "%a" Ledger_names.pp a)
            ann
            (fun fmt -> function
               | None -> Format.fprintf fmt "NONE"
               | Some a -> Format.fprintf fmt "%a" Ledgerwallet_tezos.pp_curve a)
            curr

let id_of_pk_uri (uri : pk_uri) = id_of_uri (uri :> Uri.t)
let id_of_sk_uri (uri : sk_uri) = id_of_uri (uri :> Uri.t)

let sk_or_alias_param next =
  let name = "account-alias-or-ledger-uri" in
  let desc = "An imported ledger alias or a ledger URI (e.g. 'ledger://animal/curve/path')." in
  let open Clic in
  (* Order of parsers is important: The secret key parser accepts far more inputs so must come last. *)
  param ~name ~desc (compose_parameters
                       (map_parameter ~f:(fun (_, (x, _)) -> `Pk_uri x) (Public_key.alias_parameter ()))
                       (map_parameter ~f:(fun x -> `Sk_uri x) (Client_keys.sk_uri_parameter ())))
    next

let id_of_sk_or_pk = function
  | `Sk_uri sk -> id_of_sk_uri sk
  | `Pk_uri pk -> id_of_pk_uri pk

let wrap_ledger_cmd f =
  let buf = Buffer.create 100 in
  let pp = Format.formatter_of_buffer buf in
  let res = f pp in
  debug "%s" (Buffer.contents buf) ;
  match res with
  | Error err ->
      fail (LedgerError err)
  | Ok v ->
      return v

let public_key_returning_instruction which
    ?(prompt=false)
    ledger curve path =
  let path = tezos_root @ path in
  begin match which with
    | `Get_public_key -> wrap_ledger_cmd begin fun pp ->
        Ledgerwallet_tezos.get_public_key ~prompt ~pp ledger curve path
      end
    | `Authorize_baking ->
        wrap_ledger_cmd begin fun pp ->
          Ledgerwallet_tezos.authorize_baking ~pp ledger curve path
        end
    | `Setup (main_chain_id, main_hwm, test_hwm) ->
        wrap_ledger_cmd begin fun pp ->
          Ledgerwallet_tezos.setup_baking ~pp ledger curve path
            ~main_chain_id ~main_hwm ~test_hwm
        end
  end >>|? fun pk ->
  let pk = Cstruct.to_bigarray pk in
  match curve with
  | Ledgerwallet_tezos.Ed25519 ->
      MBytes.set_int8 pk 0 0 ; (* hackish, but works. *)
      Data_encoding.Binary.of_bytes_exn Signature.Public_key.encoding pk
  | Secp256k1 ->
      let open Libsecp256k1.External in
      let buf = MBytes.create (Key.compressed_pk_bytes + 1) in
      let pk = Key.read_pk_exn secp256k1_ctx pk in
      MBytes.set_int8 buf 0 1 ;
      let _nb_written = Key.write secp256k1_ctx ~pos:1 buf pk in
      Data_encoding.Binary.of_bytes_exn Signature.Public_key.encoding buf
  | Secp256r1 ->
      let open Uecc in
      let pklen = compressed_size secp256r1 in
      let buf = MBytes.create (pklen + 1) in
      match pk_of_bytes secp256r1 pk with
      | None ->
          Pervasives.failwith "Impossible to read P256 public key from Ledger"
      | Some pk ->
          MBytes.set_int8 buf 0 2 ;
          let _nb_written = write_key ~compress:true (MBytes.sub buf 1 pklen) pk in
          Data_encoding.Binary.of_bytes_exn Signature.Public_key.encoding buf

let get_public_key = public_key_returning_instruction `Get_public_key

module Ledger = struct
  type t = {
    device_info : Hidapi.device_info ;
    version : Ledgerwallet_tezos.Version.t ;
    git_commit : string option ;
    of_curve : (Ledgerwallet_tezos.curve *
                (Signature.Public_key.t *
                 Signature.Public_key_hash.t)) list ;
    of_pkh : (Signature.Public_key_hash.t *
              (Signature.Public_key.t *
               Ledgerwallet_tezos.curve)) list ;
  }

  let create ?git_commit ~device_info ~version ~of_curve ~of_pkh () =
    { device_info ; version ; git_commit ; of_curve ; of_pkh }

  let curves { Ledgerwallet_tezos.Version.major ; minor ; patch ; _ } =
    let open Ledgerwallet_tezos in
    if (major, minor, patch) <= (0, 1, 0) then
      [ Ed25519 ; Secp256k1 ]
    else
      [ Ed25519 ; Secp256k1 ; Secp256r1 ]

  let animals_of_pkh pkh =
    pkh |> Signature.Public_key_hash.to_string |>
    Ledger_names.crouching_tiger

  let find_ledgers ?id ?git_commit h device_info version =
    fold_left_s begin fun (ledger_found, of_curve, of_pkh) curve ->
      get_public_key h curve [] >>|? fun pk ->
      let cur_pkh = Signature.Public_key.hash pk in
      let cur_animals = animals_of_pkh cur_pkh in
      log_info "Found PK: %a" Signature.Public_key.pp pk ;
      log_info "Found PKH: %a" Signature.Public_key_hash.pp cur_pkh ;
      log_info "Found Animals: %a" Ledger_names.pp cur_animals ;
      ledger_found ||
      (match id with
       | Some (Pkh pkh) when pkh = cur_pkh -> true
       | Some (Animals (animals, _)) when animals = cur_animals -> true
       | _ -> false),
      (curve, (pk, cur_pkh)) :: of_curve,
      (cur_pkh, (pk, curve)) :: of_pkh
    end (false, [], []) (curves version)
    >>=? fun (ledger_found, of_curve, of_pkh) ->
    match id with
    | None ->
        return_some
          (create ?git_commit ~device_info ~version
             ~of_curve ~of_pkh ())
    | Some _ when ledger_found ->
        return_some
          (create ?git_commit ~device_info ~version
             ~of_curve ~of_pkh ())
    | _ -> return_none

  let of_hidapi ?id device_info h =
    let buf = Buffer.create 100 in
    let pp = Format.formatter_of_buffer buf in
    let version = Ledgerwallet_tezos.get_version ~pp h in
    debug "%s" (Buffer.contents buf) ;
    match version with
    | Error (AppError { status = Ledgerwallet.Transport.Status.Ins_not_supported ; _ })
    | Error (AppError { status = Ledgerwallet_tezos.Version.Tezos_impossible_to_read_version ; _ }) ->
        (* version is < 0.1.1. Assume it is 0.0.1, Tezos app. *)
        let version =
          { Ledgerwallet_tezos.Version.app_class = Tezos ;
            major = 0 ;
            minor = 0 ;
            patch = 1 ;
          } in
        warn "Impossible to read Tezos version, assuming %a"
          Ledgerwallet_tezos.Version.pp version ;
        find_ledgers ?id h device_info version
    | Error e ->
        warn "WARNING:@ The device at [%s] is not a Tezos application@ \
              %a"
          device_info.Hidapi.path
          Ledgerwallet.Transport.pp_error e ;
        return_none
    | Ok ({ major; minor; patch; _ } as version) ->
        log_info "Found a %a application at [%s]"
          Ledgerwallet_tezos.Version.pp version device_info.path ;
        begin
          if (major, minor, patch) >= (1, 4, 0) then
            wrap_ledger_cmd (fun pp ->
                Ledgerwallet_tezos.get_git_commit ~pp h) >>=? fun c ->
            return_some c
          else return_none
        end >>=? fun git_commit ->
        find_ledgers ?id ?git_commit h device_info version
end

let find_ledgers ?id () =
  let ledgers = Hidapi.enumerate ~vendor_id ~product_id () in
  log_info "Found %d Ledger(s)" (List.length ledgers) ;
  filter_map_s begin fun device_info ->
    log_info "Processing Ledger at path [%s]" device_info.Hidapi.path ;
    (* HID interfaces get the number 0
       (cf. https://github.com/LedgerHQ/ledger-nano-s/issues/48)
       *BUT* on MacOSX the Hidapi library does not report the interface-number
       so we look at the usage-page (which is even more unspecified but used by
       prominent Ledger users:
       https://github.com/LedgerHQ/ledgerjs/commit/333ade0d55dc9c59bcc4b451cf7c976e78629681).
    *)
    if
      (device_info.Hidapi.interface_number = 0)
      ||
      (device_info.Hidapi.interface_number = -1
       && device_info.Hidapi.usage_page = 0xffa0)
    then
      begin match Hidapi.(open_path device_info.path) with
        | None -> return_none
        | Some h ->
            Lwt.finalize
              (fun () -> Ledger.of_hidapi ?id device_info h)
              (fun () -> Hidapi.close h ; Lwt.return_unit)
      end
    else
      return_none
  end ledgers

let with_ledger id f =
  find_ledgers ~id () >>=? function
  | [] ->
      failwith "No Ledger found for %a" pp_id id
  | { device_info ; version ; of_curve ; of_pkh ; _ } :: _ ->
      match Hidapi.open_path device_info.path with
      | None ->
          failwith "Cannot open Ledger %a at path %s"
            pp_id id device_info.path
      | Some h ->
          Lwt.finalize
            (fun () -> f h version of_curve of_pkh)
            (fun () -> Hidapi.close h; Lwt.return_unit)

let int32_of_path_element x =
  match Int32.of_string_opt x with
  | Some i -> Some i
  | None ->
      let len = String.length x in
      if len < 2 then None else
        Option.map
          ~f:hard (Int32.of_string_opt (String.sub x 0 (len - 1)))

let int32_of_path_element_exn x =
  match int32_of_path_element x with
  | None -> invalid_arg "int32_of_path_element_exn"
  | Some p -> p

let neuterize (sk : sk_uri) = return (make_pk_uri (sk :> Uri.t))

let path_of_sk_uri (uri : sk_uri) =
  match TzString.split_path (Uri.path (uri :> Uri.t)) with
  | [] -> []
  | curve :: path when Ledgerwallet_tezos.curve_of_string curve <> None ->
      List.map int32_of_path_element_exn path
  | path -> List.map int32_of_path_element_exn path

let path_of_pk_uri (uri : pk_uri) =
  match TzString.split_path (Uri.path (uri :> Uri.t)) with
  | [] -> []
  | curve :: path when Ledgerwallet_tezos.curve_of_string curve <> None ->
      List.map int32_of_path_element_exn path
  | path -> List.map int32_of_path_element_exn path

let unopt_curve annimal = function
  | Some curve -> return curve
  | None ->
      failwith "A curve specification is required for this operation,@ e.g.@ \
                \"ledger://%a/{ed25519,...}\"" Ledger_names.pp annimal

let public_key
    ?(interactive : Client_context.io_wallet option) (pk_uri : pk_uri) =
  let find_curve of_pkh = function
    | Pkh pkh ->
        protect (fun () -> return (snd (List.assoc pkh of_pkh)))
    | Animals (a, curve_opt) -> unopt_curve a curve_opt
  in
  match Hashtbl.find_opt pks pk_uri with
  | Some pk -> return pk
  | None ->
      id_of_pk_uri pk_uri >>=? fun id ->
      with_ledger id begin fun ledger _version _of_curve of_pkh  ->
        find_curve of_pkh id >>=? fun curve  ->
        let path = path_of_pk_uri pk_uri in
        begin
          match interactive with
          | Some cctxt ->
              get_public_key ~prompt:false ledger curve path >>=? fun pk ->
              let pkh = Signature.Public_key.hash pk in
              cctxt#message
                "Please validate@ (and write down)@ the public key hash\
                 @ displayed@ on the Ledger,@ it should be equal@ to `%a`:"
                Signature.Public_key_hash.pp pkh >>= fun () ->
              get_public_key ~prompt:true ledger curve path
          | None ->
              get_public_key ~prompt:false ledger curve path
        end >>=? fun pk ->
        let pkh = Signature.Public_key.hash pk in
        Hashtbl.replace pks pk_uri pk ;
        Hashtbl.replace pkhs pk_uri pkh ;
        return pk
      end >>= function
      | Error err -> failwith "%a" pp_print_error err
      | Ok v -> return v

let public_key_hash ?interactive pk_uri =
  match Hashtbl.find_opt pkhs pk_uri with
  | Some pkh -> return (pkh, None)
  | None ->
      public_key ?interactive pk_uri >>=? fun pk ->
      return (Hashtbl.find pkhs pk_uri, Some pk)

let curve_of_id = function
  | Pkh pkh -> return (curve_of_pkh pkh)
  | Animals (a, curve_opt) -> unopt_curve a curve_opt

(* The Ledger uses a special value 0x00000000 for the “any” chain-id: *)
let pp_ledger_chain_id fmt s =
  match s with
  | "\x00\x00\x00\x00" -> Format.fprintf fmt "'Unspecified'"
  | other -> Format.fprintf fmt "%a" Chain_id.pp (Chain_id.of_string_exn other)

let sign ?watermark sk_uri msg =
  id_of_sk_uri sk_uri >>=? fun id ->
  with_ledger id begin fun ledger { major; minor; patch; _ } _of_curve _of_pkh ->
    let msg = Option.unopt_map watermark
        ~default:msg ~f:begin fun watermark ->
        MBytes.concat "" [Signature.bytes_of_watermark watermark ;
                          msg]
      end in
    curve_of_id id >>=? fun curve ->
    let path = tezos_root @ path_of_sk_uri sk_uri in
    let msg_len = MBytes.length msg in
    wrap_ledger_cmd begin fun pp ->
      if msg_len > 1024 && (major, minor, patch) < (1, 1, 0) then
        Ledgerwallet_tezos.sign ~hash_on_ledger:false
          ~pp ledger curve path
          (Cstruct.of_bigarray (Blake2B.(to_bytes (hash_bytes [ msg ]))))
      else
        Ledgerwallet_tezos.sign
          ~pp ledger curve path (Cstruct.of_bigarray msg)
    end >>=? fun signature ->
    match curve with
    | Ed25519 ->
        let signature = Cstruct.to_bigarray signature in
        let signature = Ed25519.of_bytes_exn signature in
        return (Signature.of_ed25519 signature)
    | Secp256k1 ->
        (* Remove parity info *)
        Cstruct.(set_uint8 signature 0 (get_uint8 signature 0 land 0xfe)) ;
        let signature = Cstruct.to_bigarray signature in
        let open Libsecp256k1.External in
        let signature = Sign.read_der_exn secp256k1_ctx signature in
        let bytes = Sign.to_bytes secp256k1_ctx signature in
        let signature = Secp256k1.of_bytes_exn bytes in
        return (Signature.of_secp256k1 signature)
    | Secp256r1 ->
        (* Remove parity info *)
        Cstruct.(set_uint8 signature 0 (get_uint8 signature 0 land 0xfe)) ;
        let signature = Cstruct.to_bigarray signature in
        let open Libsecp256k1.External in
        (* We use secp256r1 library to extract P256 DER signature. *)
        let signature = Sign.read_der_exn secp256k1_ctx signature in
        let buf = Sign.to_bytes secp256k1_ctx signature in
        let signature = P256.of_bytes_exn buf in
        return (Signature.of_p256 signature)
  end


let deterministic_nonce _ _ = fail Ledger_deterministic_nonce_not_implemented
let deterministic_nonce_hash _ _ = fail Ledger_deterministic_nonce_not_implemented
let supports_deterministic_nonces _ = return_false

let commands =
  let open Clic in
  let group =
    { Clic.name = "ledger" ;
      title = "Commands for managing the connected Ledger Nano S devices" } in
  fun () -> [
      Clic.command ~group
        ~desc: "List supported Ledger Nano S devices connected."
        no_options
        (fixed [ "list" ; "connected" ; "ledgers" ])
        (fun () (cctxt : Client_context.full) ->
           find_ledgers () >>=? function
           | [] ->
               cctxt#message "No device found." >>= fun () ->
               cctxt#message "Make sure a Ledger Nano S is connected and in the Tezos Wallet or Tezos Baking app." >>= fun () ->
               return_unit
           | ledgers ->
               iter_s begin fun { Ledger.device_info = { Hidapi.path ;
                                                         manufacturer_string ;
                                                         product_string ; _ } ;
                                  of_curve ; version ; git_commit ; _ } ->
                 let manufacturer = Option.unopt ~default:"(none)" manufacturer_string in
                 let product = Option.unopt ~default:"(none)" product_string in
                 cctxt#message "Found a %a (commit %s) application running on %s %s at [%s]."
                   Ledgerwallet_tezos.Version.pp version
                   (match git_commit with None -> "unknown" | Some c -> c)
                   manufacturer product path >>= fun () ->
                 let of_curve = List.rev of_curve in
                 begin match List.hd_opt of_curve with
                   | None ->
                       failwith "No curve available, upgrade Ledger software"
                   | Some (_, (_, pkh)) ->
                       return (Ledger_names.crouching_tiger
                                 (Signature.Public_key_hash.to_string pkh))
                 end >>=? fun animals ->
                 cctxt#message
                   "@[<v 0>@,To use keys at BIP32 path \
                    m/44'/1729'/0'/0' (default Tezos key path), use \
                    one of@, @[<v 0>%a@]@]"
                   (Format.pp_print_list
                      (fun ppf (curve, _) ->
                         Format.fprintf ppf
                           "tezos-client import secret key \
                            ledger_%s \"ledger://%a/0'/0'\""
                           (Sys.getenv "USER")
                           pp_id (Animals (animals, Some curve))))
                   of_curve >>= fun () ->
                 return_unit
               end ledgers) ;

      Clic.command ~group
        ~desc: "Display version/public-key/address information for a Ledger URI"
        (args1 (switch ~doc:"Test signing operation" ~long:"test-sign" ()))
        (prefixes [ "show" ; "ledger" ]
         @@ Client_keys.sk_uri_param
         @@ stop)
        (fun test_sign sk_uri (cctxt : Client_context.full) ->
           neuterize sk_uri >>=? fun pk_uri ->
           id_of_pk_uri pk_uri >>=? fun id ->
           find_ledgers ~id () >>=? function
           | [] ->
               failwith "No ledger found for %a" pp_id id
           | { Ledger.device_info ; version ; _ } :: _ ->
               let manufacturer =
                 Option.unopt ~default:"(none)" device_info.manufacturer_string in
               let product =
                 Option.unopt ~default:"(none)" device_info.product_string in
               cctxt#message
                 "Found a %a application running on a \
                  %s %s at [%s]."
                 Ledgerwallet_tezos.Version.pp version
                 manufacturer product device_info.path >>= fun () ->
               begin match id with
                 | (Pkh _ | Animals (_, Some _)) -> (* → Can public keys. *)
                     public_key pk_uri >>=? fun pk ->
                     public_key_hash pk_uri >>=? fun (pkh, _) ->
                     cctxt#message
                       "@[<v 0>Tezos address at this path/curve: %a@,\
                        Corresponding full public key: %a@]"
                       Signature.Public_key_hash.pp pkh
                       Signature.Public_key.pp pk >>= fun () ->
                     begin match test_sign, version.app_class with
                       | true, Tezos ->
                           let pkh_bytes = Signature.Public_key_hash.to_bytes pkh in
                           (* Signing requires validation on the device.  *)
                           cctxt#message "Attempting a signature, please \
                                          validate on the ledger." >>= fun () ->
                           sign ~watermark:Generic_operation
                             sk_uri pkh_bytes >>=? fun signature ->
                           begin match Signature.check ~watermark:Generic_operation
                                         pk signature pkh_bytes with
                           | false ->
                               failwith "Fatal: Ledger cannot sign with %a"
                                 Signature.Public_key_hash.pp pkh
                           | true ->
                               cctxt#message "Tezos Wallet successfully signed."
                               >>= fun () ->
                               return_unit
                           end
                       | true, TezBake ->
                           failwith "Option --test-sign only works \
                                     for the Tezos Wallet app."
                       | false, _ ->
                           return_unit
                     end
                 | Animals (_, None) when test_sign ->
                     failwith "Option --test-sign only works \
                               for the Tezos Wallet app with a \
                               curve/path specification."
                 | Animals (_, None) ->
                     cctxt#message "No curve was provided, \
                                    there is no Tezos-address/public-key \
                                    to show/test."
                     >>= fun () ->
                     return_unit
               end
        ) ;

      Clic.command ~group
        ~desc: "Query the path of the authorized key"
        no_options
        (prefixes [ "get" ; "ledger" ; "authorized" ; "path" ; "for" ]
         @@ sk_or_alias_param
         @@ stop)
        (fun () uri (cctxt : Client_context.full) ->
           id_of_sk_or_pk uri >>=? fun root_id ->
           with_ledger root_id begin fun h version _of_curve _to_curve ->
             (if version.major < 2 then
                wrap_ledger_cmd (fun pp -> Ledgerwallet_tezos.get_authorized_key ~pp h)
                >>|? fun path -> (path, None)
              else
                wrap_ledger_cmd (fun pp -> Ledgerwallet_tezos.get_authorized_path_and_curve ~pp h)
                >>= function
                | Error (LedgerError (AppError {status = Ledgerwallet.Transport.Status.Referenced_data_not_found; _}) :: _) -> return ([], None)
                | Error _ as e -> Lwt.return e
                | Ok (path, curve) -> return (path, Some curve))
             >>=? function
             | ([], _) ->
                 cctxt#message
                   "@[<v 0>No baking key authorized for %a@]" pp_id root_id
                 >>= fun () ->
                 return_unit
             | (path, None) ->
                 cctxt#message
                   "@[<v 0>Authorized baking path: %a@]"
                   Bip32_path.pp_path path >>= fun () ->
                 return_unit
             | (path, Some curve) ->
                 cctxt#message
                   "@[<v 0>Authorized baking path: %a@]"
                   Bip32_path.pp_path path >>= fun () ->
                 cctxt#message
                   "@[<v 0>Authorized baking curve: %a@]"
                   Ledgerwallet_tezos.pp_curve curve >>= fun () ->
                 (match root_id with
                  | Pkh _ -> cctxt#message "@[<v 0>Authorized baking PKH: %a@]"
                               pp_id root_id
                  | Animals (cthd, _) -> cctxt#message "@[<v 0>Authorized baking URI: %a@]"
                                           pp_animals_uri (cthd, curve, path))
                 >>= fun () ->
                 return_unit
           end) ;

      Clic.command ~group
        ~desc: "Authorize a Ledger to bake for a key (deprecated, \
                use `setup ledger ...` with recent versions of the Baking app)"
        no_options
        (prefixes [ "authorize" ; "ledger" ; "to" ; "bake" ; "for" ]
         @@ Public_key.alias_param
         @@ stop)
        (fun () (_, (pk_uri, _)) (cctxt : Client_context.full) ->
           id_of_pk_uri pk_uri >>=? fun root_id ->
           with_ledger root_id begin fun h version _of_curve _of_pkh ->
             begin match version with
               | { Ledgerwallet_tezos.Version.app_class = Tezos ; _ } ->
                   failwith "This command (`authorize ledger ...`) only \
                             works with the Tezos Baking app"
               | { Ledgerwallet_tezos.Version.app_class = TezBake ;
                   major ; _ } when major >= 2 ->
                   failwith
                     "This command (`authorize ledger ...`) is@ \
                      not compatible with@ this version of the Ledger@ \
                      Baking app (%a >= 2.0.0),@ please use the command@ \
                      `setup ledger to bake for ...`@ from now on."
                     Ledgerwallet_tezos.Version.pp version
               | _ ->
                   cctxt#message
                     "This Ledger Baking app is outdated (%a)@ running@ \
                      in backwards@ compatibility mode."
                     Ledgerwallet_tezos.Version.pp version
                   >>= fun () ->
                   return_unit
             end
             >>=? fun () ->
             let path = path_of_pk_uri pk_uri in
             curve_of_id root_id >>=? fun curve ->
             public_key_returning_instruction `Authorize_baking h curve path
             >>=? fun pk ->
             let pkh = Signature.Public_key.hash pk in
             cctxt#message
               "@[<v 0>Authorized baking for address: %a@,\
                Corresponding full public key: %a@]"
               Signature.Public_key_hash.pp pkh
               Signature.Public_key.pp pk >>= fun () ->
             return_unit
           end) ;

      Clic.command ~group
        ~desc: "Setup a Ledger to bake for a key"
        (let hwm_arg kind =
           let doc =
             Printf.sprintf
               "Use <HWM> as %s chain high watermark instead of asking the ledger."
               kind in
           let long = kind ^ "-hwm" in
           default_arg ~doc ~long ~placeholder:"HWM"
             ~default:"ASK-LEDGER"
             (parameter
                (fun _ -> function
                   | "ASK-LEDGER" -> return None
                   | s ->
                       try return (Some (Int32.of_string s)) with _ ->
                         failwith "Parameter %S should be a 32-bits integer" s))
         in
         args3
           (default_arg
              ~doc:"Use <ID> as main chain-id instead of asking the node."
              ~long:"main-chain-id" ~placeholder:"ID"
              ~default:"ASK-NODE"
              (parameter
                 (fun _ -> function
                    | "ASK-NODE" -> return `Ask_node
                    | s ->
                        try return (`Int32 (Int32.of_string s))
                        with _ ->
                          (try return (`Chain_id (Chain_id.of_b58check_exn s))
                           with _ ->
                             failwith "Parameter %S should be a 32-bits integer \
                                       or a Base58 chain-id" s))))
           (hwm_arg "main") (hwm_arg "test"))
        (prefixes [ "setup" ; "ledger" ; "to" ; "bake" ; "for" ]
         @@ Public_key.alias_param
         @@ stop)
        (fun (chain_id_opt, main_hwm_opt, test_hwm_opt)
          (_, (pk_uri, _)) (cctxt : Client_context.full) ->
          id_of_pk_uri pk_uri >>=? fun root_id ->
          with_ledger root_id begin fun h version _of_curve _of_pkh ->
            begin
              let open Ledgerwallet_tezos.Version in
              match version with
              | { app_class = Tezos ; _ } ->
                  failwith "This command (`setup ledger ...`) only \
                            works with the Tezos Baking app"
              | { app_class = TezBake ;
                  major ; _ } when major < 2 ->
                  failwith
                    "This command (`setup ledger ...`)@ is not@ compatible@ with \
                     this version@ of the Ledger Baking app@ (%a < 2.0.0),@ \
                     please upgrade@ your ledger@ or use the command@ \
                     `authorize ledger to bake for ...`"
                    pp version
              | _ -> return_unit
            end
            >>=? fun () ->
            let chain_id_of_int32 i32 =
              let open Int32 in
              let byte n =
                logand 0xFFl (shift_right i32 (n * 8))
                |> Int32.to_int |> char_of_int in
              Chain_id.of_string_exn
                (Stringext.of_array (Array.init 4 (fun i -> byte (3 - i)))) in
            begin match chain_id_opt with
              | `Ask_node ->
                  Chain_services.chain_id cctxt ()
              | `Int32 s -> return (chain_id_of_int32 s)
              | `Chain_id chid -> return chid
            end
            >>=? fun main_chain_id ->
            let path = path_of_pk_uri pk_uri in
            curve_of_id root_id >>=? fun curve ->
            wrap_ledger_cmd begin fun pp ->
              Ledgerwallet_tezos.get_all_high_watermarks ~pp h
            end
            >>=? fun (`Main_hwm current_mh, `Test_hwm current_th, `Chain_id current_ci) ->
            let main_hwm = Option.unopt main_hwm_opt ~default:current_mh in
            let test_hwm = Option.unopt test_hwm_opt ~default:current_th in
            cctxt#message "Setting up the ledger:@.\
                           * Main chain ID: %a -> %a@.\
                           * Main chain High Watermark: %ld -> %ld@.\
                           * Test chain High Watermark: %ld -> %ld"
              pp_ledger_chain_id current_ci
              Chain_id.pp main_chain_id
              current_mh main_hwm
              current_th test_hwm
            >>= fun () ->
            public_key_returning_instruction
              (`Setup (Chain_id.to_string main_chain_id, main_hwm, test_hwm))
              h curve path
            >>=? fun pk ->
            let pkh = Signature.Public_key.hash pk in
            cctxt#message
              "@[<v 0>Authorized baking for address: %a@,\
               Corresponding full public key: %a@]"
              Signature.Public_key_hash.pp pkh
              Signature.Public_key.pp pk >>= fun () ->
            return_unit
          end) ;

      Clic.command ~group
        ~desc: "Deauthorize Ledger from baking"
        no_options
        (prefixes [ "deauthorize" ; "ledger" ; "baking" ; "for" ]
         @@ sk_or_alias_param
         @@ stop)
        (fun () uri (_ : Client_context.full) ->
           id_of_sk_or_pk uri >>=? fun id ->
           with_ledger id begin fun h version _ _ ->
             match version.app_class with
             | Tezos ->
                 failwith "Fatal: this operation is only valid with the \
                           Tezos Baking application"
             | TezBake when version.major < 2 ->
                 failwith "Fatal: this operation is only available with \
                           Tezos Baking application version 2 or higher"
             | TezBake ->
                 wrap_ledger_cmd begin fun pp ->
                   Ledgerwallet_tezos.deauthorize_baking ~pp h
                 end
           end
        );

      Clic.command ~group
        ~desc: "Get high water mark of a Ledger"
        (args1 (switch ~doc:"Prevent the fallback to the (deprecated) Ledger \
                             instructions (for 1.x.y versions of the Baking app)"
                  ~long:"no-legacy-instructions" ()))
        (prefixes [ "get" ; "ledger" ; "high" ; "watermark" ; "for" ]
         @@ sk_or_alias_param
         @@ stop)
        (fun no_legacy_apdu uri (cctxt : Client_context.full) ->
           id_of_sk_or_pk uri >>=? fun id ->
           with_ledger id begin fun h version _ _ ->
             match version.app_class with
             | Tezos ->
                 failwith "Fatal: this operation is only valid with the \
                           Tezos Baking application"
             | TezBake when not no_legacy_apdu && version.major < 2 ->
                 wrap_ledger_cmd begin fun pp ->
                   Ledgerwallet_tezos.get_high_watermark ~pp h
                 end
                 >>=? fun hwm ->
                 cctxt#message "The high water mark for@ %a@ is %ld."
                   pp_id id hwm >>= fun () ->
                 return_unit
             | TezBake when no_legacy_apdu && version.major < 2 ->
                 failwith
                   "Cannot get the high water mark with@ \
                    `--no-legacy-instructions` and version %a"
                   Ledgerwallet_tezos.Version.pp version
             | TezBake ->
                 wrap_ledger_cmd begin fun pp ->
                   Ledgerwallet_tezos.get_all_high_watermarks ~pp h
                 end
                 >>=? fun (`Main_hwm mh, `Test_hwm th, `Chain_id ci) ->
                 cctxt#message
                   "The high water mark values for@ %a@ are\
                    @ %ld for the main-chain@ (%a)@ \
                    and@ %ld for the test-chain."
                   pp_id id mh pp_ledger_chain_id ci th
                 >>= fun () ->
                 return_unit
           end
        ) ;

      Clic.command ~group
        ~desc: "Set high water mark of a Ledger"
        no_options
        (prefixes [ "set" ; "ledger" ; "high" ; "watermark" ; "for" ]
         @@ sk_or_alias_param
         @@ (prefix "to")
         @@ (param
               ~name: "high watermark"
               ~desc: "High watermark"
               (parameter (fun _ctx s ->
                    try return (Int32.of_string s)
                    with _ -> failwith "%s is not an int32 value" s)))
         @@ stop)
        (fun () uri hwm (cctxt : Client_context.full) ->
           id_of_sk_or_pk uri >>=? fun id ->
           with_ledger id begin fun h version _ _ ->
             match version.app_class with
             | Tezos ->
                 failwith "Fatal: this operation is only valid with TezBake"
             | TezBake ->
                 wrap_ledger_cmd begin fun pp ->
                   Ledgerwallet_tezos.set_high_watermark ~pp h hwm
                 end >>=? fun () ->
                 wrap_ledger_cmd begin fun pp ->
                   Ledgerwallet_tezos.get_high_watermark ~pp h
                 end >>=? fun new_hwm ->
                 cctxt#message
                   "@[<v 0>%a has now high water mark: %ld@]"
                   pp_id id new_hwm >>= fun () ->
                 return_unit
           end
        ) ;
    ]
