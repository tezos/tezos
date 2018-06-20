(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys

include Logging.Make(struct let name = "client.signer.ledger" end)

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
let tezos_root = [hard 44l ; hard 1729l]

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

let get_public_key ledger curve path =
  let path = tezos_root @ path in
  let buf = Buffer.create 100 in
  let pp = Format.formatter_of_buffer buf in
  let pk = Ledgerwallet_tezos.get_public_key ~pp ledger curve path in
  Format.pp_print_flush pp () ;
  debug "%s" (Buffer.contents buf) ;
  let pk = Cstruct.to_bigarray pk in
  match curve with
  | Ed25519 ->
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

module Ledger = struct
  type t = {
    device_info : Hidapi.device_info ;
    of_curve : (Ledgerwallet_tezos.curve * (Signature.Public_key.t *
                                            Signature.Public_key_hash.t)) list ;
    of_pkh : (Signature.Public_key_hash.t * (Signature.Public_key.t *
                                             Ledgerwallet_tezos.curve)) list ;
  }

  let create ~device_info ~of_curve ~of_pkh =
    { device_info ; of_curve ; of_pkh }

  let of_hidapi ?pkh device_info h =
    let curves = [ Ledgerwallet_tezos.Ed25519 ; Secp256k1 ; Secp256r1 ] in
    let pkh_found, of_curve, of_pkh =
      List.fold_left begin fun (pkh_found, of_curve, of_pkh) curve ->
        let pk = get_public_key h curve [] in
        let cur_pkh = Signature.Public_key.hash pk in
        pkh_found ||
        Option.unopt_map pkh ~default:false ~f:(fun pkh -> pkh = cur_pkh),
        (curve, (pk, cur_pkh)) :: of_curve,
        (cur_pkh, (pk, curve)) :: of_pkh
      end (false, [], []) curves in
    match pkh with
    | None -> return (Some (create ~device_info ~of_curve ~of_pkh))
    | Some _ when pkh_found ->
        return (Some (create ~device_info ~of_curve ~of_pkh))
    | _ -> return None
end

let find_ledgers ?pkh () =
  let ledgers = Hidapi.enumerate ~vendor_id ~product_id () in
  filter_map_s begin fun device_info ->
    match Hidapi.(open_path device_info.path) with
    | None -> return None
    | Some h ->
        Lwt.finalize
          (fun () -> Ledger.of_hidapi ?pkh device_info h)
          (fun () -> Hidapi.close h ; Lwt.return_unit)
  end ledgers

let with_ledger pkh f =
  find_ledgers ~pkh () >>=? function
  | [] ->
      failwith "No Ledger found for %a" Signature.Public_key_hash.pp pkh
  | { device_info ; of_curve ; of_pkh } :: _ ->
      match Hidapi.open_path device_info.path with
      | None ->
          failwith "Cannot open Ledger %a at path %s"
            Signature.Public_key_hash.pp pkh device_info.path
      | Some h ->
          Lwt.finalize
            (fun () -> f h of_curve of_pkh)
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

let pkh_of_pk_uri (uri : pk_uri) =
  let uri = (uri :> Uri.t) in
  match Option.apply (Uri.host uri)
          ~f:Signature.Public_key_hash.of_b58check_opt with
  | None ->
      failwith "No public key hash in %a" Uri.pp_hum uri
  | Some pkh -> return pkh

let pkh_of_sk_uri (uri : sk_uri) =
  let uri = (uri :> Uri.t) in
  match Option.apply (Uri.host uri)
          ~f:Signature.Public_key_hash.of_b58check_opt with
  | None ->
      failwith "No public key hash in %a" Uri.pp_hum uri
  | Some pkh -> return pkh

let path_of_sk_uri (uri : sk_uri) =
  TzString.split_path (Uri.path (uri :> Uri.t)) |>
  List.map int32_of_path_element_exn

let path_of_pk_uri (uri : pk_uri) =
  TzString.split_path (Uri.path (uri :> Uri.t)) |>
  List.map int32_of_path_element_exn

let public_key (pk_uri : pk_uri) =
  match Hashtbl.find_opt pks pk_uri with
  | Some pk -> return pk
  | None ->
      pkh_of_pk_uri pk_uri >>=? fun pkh ->
      with_ledger pkh begin fun ledger _of_curve of_pkh ->
        let _root_pk, curve = List.assoc pkh of_pkh in
        let path = path_of_pk_uri pk_uri in
        let pk = get_public_key ledger curve path in
        let pkh = Signature.Public_key.hash pk in
        Hashtbl.replace pks pk_uri pk ;
        Hashtbl.replace pkhs pk_uri pkh ;
        return pk
      end >>= function
      | Error err -> failwith "%a" pp_print_error err
      | Ok v -> return v

let public_key_hash pk_uri =
  match Hashtbl.find_opt pkhs pk_uri with
  | Some pkh -> return (pkh, None)
  | None ->
      public_key pk_uri >>=? fun pk ->
      return (Hashtbl.find pkhs pk_uri, Some pk)

let sign ?watermark sk_uri msg =
  pkh_of_sk_uri sk_uri >>=? fun pkh ->
  with_ledger pkh begin fun ledger _of_curve _of_pkh ->
    let msg = Option.unopt_map watermark
        ~default:msg ~f:begin fun watermark ->
        MBytes.concat "" [Signature.bytes_of_watermark watermark ;
                          msg]
      end in
    let curve = curve_of_pkh pkh in
    let path = tezos_root @ path_of_sk_uri sk_uri in
    let buf = Buffer.create 100 in
    let pp = Format.formatter_of_buffer buf in
    let signature = Ledgerwallet_tezos.sign ~pp ledger curve path (Cstruct.of_bigarray msg) in
    Format.pp_print_flush pp () ;
    debug "%s" (Buffer.contents buf) ;
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
        (fun () (cctxt : Client_context.io_wallet) ->
           find_ledgers () >>=? function
           | [] ->
               cctxt#message "No device found." >>= fun () ->
               cctxt#message "Make sure a Ledger Nano S is connected and in the Tezos Wallet app." >>= fun () ->
               return ()
           | ledgers ->
               iter_s begin fun { Ledger.device_info = { Hidapi.path ;
                                                         manufacturer_string ;
                                                         product_string ; _ } ;
                                  of_curve ; _ } ->
                 let manufacturer = Option.unopt ~default:"(none)" manufacturer_string in
                 let product = Option.unopt ~default:"(none)" product_string in
                 cctxt#message "Found a valid Tezos application running on %s %s at [%s]."
                   manufacturer product path >>= fun () ->
                 let of_curve = List.rev of_curve in
                 cctxt#message
                   "@[<v 0>@,To add the root key of this ledger, use one of@,\
                   \  @[<v 0>%a@]@,\
                    Each of these tz* is a valid Tezos address.@,\
                    @,\
                    To use a derived address, add a hardened BIP32 path suffix \
                    at the end of the URI.@,\
                    For instance, to use keys at BIP32 path m/44'/1729'/0'/0', use one of@,\
                   \  @[<v 0>%a@]@,\
                    In this case, your Tezos address will be a derived tz*.@,\
                    It will be displayed when you do the import, \
                    or using command `show ledger path`.@]"
                   (Format.pp_print_list
                      (fun ppf (curve, (_pk, pkh)) ->
                         Format.fprintf ppf
                           "tezos-client import secret key ledger_%s_%s ledger://%a # %s signature"
                           (Sys.getenv "USER")
                           (match curve with
                            | Ledgerwallet_tezos.Ed25519 -> "ed"
                            | Ledgerwallet_tezos.Secp256k1 -> "secp"
                            | Ledgerwallet_tezos.Secp256r1 -> "p2")
                           Signature.Public_key_hash.pp pkh
                           (match curve with
                            | Ledgerwallet_tezos.Ed25519 -> "Ed25519"
                            | Ledgerwallet_tezos.Secp256k1 -> "Secp256k1"
                            | Ledgerwallet_tezos.Secp256r1 -> "P-256")))
                   of_curve
                   (Format.pp_print_list
                      (fun ppf (curve, (_pk, pkh)) ->
                         Format.fprintf ppf
                           "tezos-client import secret key ledger_%s_%s_0_0 \"ledger://%a/0'/0'\""
                           (Sys.getenv "USER")
                           (match curve with
                            | Ledgerwallet_tezos.Ed25519 -> "ed"
                            | Ledgerwallet_tezos.Secp256k1 -> "secp"
                            | Ledgerwallet_tezos.Secp256r1 -> "p2")
                           Signature.Public_key_hash.pp pkh))
                   of_curve >>= fun () ->
                 return ()
               end ledgers) ;

      Clic.command ~group
        ~desc: "Show BIP32 derivation at path for Ledger"
        no_options
        (prefixes [ "show" ; "ledger" ; "path" ]
         @@ Client_keys.sk_uri_param
         @@ stop)
        (fun () sk_uri (cctxt : Client_context.io_wallet) ->
           neuterize sk_uri >>=? fun pk_uri ->
           pkh_of_pk_uri pk_uri >>=? fun pkh ->
           find_ledgers ~pkh () >>=? function
           | [] ->
               failwith "No ledger found for %a" Signature.Public_key_hash.pp pkh
           | { Ledger.device_info; _ } :: _ ->
               let manufacturer =
                 Option.unopt ~default:"(none)" device_info.manufacturer_string in
               let product =
                 Option.unopt ~default:"(none)" device_info.product_string in
               cctxt#message "Found a valid Tezos application running on %s %s at [%s]."
                 manufacturer product device_info.path >>= fun () ->
               public_key pk_uri >>=? fun pk ->
               public_key_hash pk_uri >>=? fun (pkh, _) ->
               let pkh_bytes = Signature.Public_key_hash.to_bytes pkh in
               sign ~watermark:Generic_operation
                 sk_uri pkh_bytes >>=? fun signature ->
               match Signature.check ~watermark:Generic_operation
                       pk signature pkh_bytes with
               | false ->
                   failwith "Fatal: Ledger cannot sign with %a"
                     Signature.Public_key_hash.pp pkh
               | true ->
                   cctxt#message
                     "@[<v 0>Tezos address at this path: %a@,\
                      Corresponding full public key: %a@]"
                     Signature.Public_key_hash.pp pkh
                     Signature.Public_key.pp pk >>= fun () ->
                   return ()
        )
    ]

