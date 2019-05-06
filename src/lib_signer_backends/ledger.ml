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

include Internal_event.Legacy_logging.Make(struct
    let name = "client.signer.ledger"
  end)

module Bip32_path = struct
  let hard = Int32.logor 0x8000_0000l
  let unhard = Int32.logand 0x7fff_ffffl
  let is_hard n = Int32.logand 0x8000_0000l n <> 0l
  let tezos_root = [hard 44l ; hard 1729l]

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

(** Wrappers around Ledger APDUs. *)
module Ledger_commands = struct

  let wrap_ledger_cmd f =
    let buf = Buffer.create 100 in
    let pp =
      Format.make_formatter
        (fun s ofs lgth -> Buffer.add_substring buf s ofs lgth)
        (fun () -> debug "%s%!" (Buffer.contents buf) ; Buffer.clear buf) in
    let res = f pp in
    lwt_debug "%!" >>= fun () ->
    match res with
    | Error err ->
        fail (LedgerError err)
    | Ok v ->
        return v

  let get_version ~device_info h =
    let buf = Buffer.create 100 in
    let pp = Format.formatter_of_buffer buf in
    let version = Ledgerwallet_tezos.get_version ~pp h in
    debug "%s" (Buffer.contents buf) ;
    match version with
    | Error e ->
        warn "WARNING:@ The device at [%s] is not a Tezos application@ \
              %a"
          device_info.Hidapi.path
          Ledgerwallet.Transport.pp_error e ;
        return_none
    | Ok version ->
        (if (version.major, version.minor) < (1, 4)
         then
           failwith
             "Version %a of the ledger apps is not supported by this client"
             Ledgerwallet_tezos.Version.pp version
         else return_unit)
        >>=? fun () ->
        wrap_ledger_cmd (fun pp ->
            Ledgerwallet_tezos.get_git_commit ~pp h) >>=? fun git_commit ->
        log_info "Found a %a application at [%s] (git-description: %S)"
          Ledgerwallet_tezos.Version.pp version device_info.path git_commit ;
        let cleaned_up =
          (* The ledger sends a NUL-terminated C-String: *)
          if git_commit.[String.length git_commit - 1] = '\x00'
          then String.sub git_commit 0 (String.length git_commit - 1)
          else git_commit in
        return_some (version, cleaned_up)

  let secp256k1_ctx =
    Libsecp256k1.External.Context.create ~sign:false ~verify:false ()

  let public_key_returning_instruction which
      ?(prompt=false)
      hidapi curve path =
    let path = Bip32_path.tezos_root @ path in
    begin match which with
      | `Get_public_key -> wrap_ledger_cmd begin fun pp ->
          Ledgerwallet_tezos.get_public_key ~prompt ~pp hidapi curve path
        end
      | `Authorize_baking ->
          wrap_ledger_cmd begin fun pp ->
            Ledgerwallet_tezos.authorize_baking ~pp hidapi curve path
          end
      | `Setup (main_chain_id, main_hwm, test_hwm) ->
          wrap_ledger_cmd begin fun pp ->
            Ledgerwallet_tezos.setup_baking ~pp hidapi curve path
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

  let pkh_of_pk = Signature.Public_key.hash

  let public_key
      ?(interactive : Client_context.io_wallet option) hid curve path =
    begin match interactive with
      | Some cctxt ->
          get_public_key ~prompt:false hid curve path >>=? fun pk ->
          let pkh = pkh_of_pk pk in
          cctxt#message
            "Please validate@ (and write down)@ the public key hash\
             @ displayed@ on the Ledger,@ it should be equal@ to `%a`:"
            Signature.Public_key_hash.pp pkh >>= fun () ->
          get_public_key ~prompt:true hid curve path
      | None ->
          get_public_key ~prompt:false hid curve path
    end

  let public_key_hash ?interactive hid curve path =
    public_key ?interactive hid curve path >>=? fun pk ->
    return (pkh_of_pk pk, pk)

  let get_authorized_path  hid version =
    let open Ledgerwallet_tezos.Version in
    if version.major < 2 then
      wrap_ledger_cmd (fun pp -> Ledgerwallet_tezos.get_authorized_key ~pp hid)
      >>|? fun path -> `Legacy_path path
    else
      wrap_ledger_cmd
        (fun pp -> Ledgerwallet_tezos.get_authorized_path_and_curve ~pp hid)
      >>= function
      | Error (LedgerError
                 (AppError
                    {status = Ledgerwallet.Transport.Status.Referenced_data_not_found; _}) :: _) ->
          return `No_baking_authorized
      | Error _ as e -> Lwt.return e
      | Ok (path, curve) -> return (`Path_curve (path, curve))

  let sign ?watermark hid curve path base_msg =
    let msg =
      Option.unopt_map watermark
        ~default:base_msg ~f:(fun watermark ->
            MBytes.concat ""
              [Signature.bytes_of_watermark watermark ; base_msg]) in
    let path = Bip32_path.tezos_root @ path in
    wrap_ledger_cmd begin fun pp ->
      (* if msg_len > 1024 && (major, minor, patch) < (1, 1, 0) then
       *   Ledgerwallet_tezos.sign ~hash_on_ledger:false
       *     ~pp ledger curve path
       *     (Cstruct.of_bigarray (Blake2B.(to_bytes (hash_bytes [ msg ]))))
       * else *)
      Ledgerwallet_tezos.sign
        ~pp hid curve path (Cstruct.of_bigarray msg)
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

(** Identification of a ledger's root key through crouching-tigers
    (not the keys used for an account). *)
module Ledger_id = struct
  (**
     The “ID” of the ledger is the animals (or pkh) corresponding to
     ["/ed25519/"] (first curve, no path).
  *)
  type t = Animals of Ledger_names.t | Pkh of Signature.public_key_hash

  let animals_of_pkh pkh =
    pkh |> Signature.Public_key_hash.to_string |>
    Ledger_names.crouching_tiger

  let curve = Ledgerwallet_tezos.Ed25519

  let get hidapi =
    Ledger_commands.get_public_key hidapi curve [] >>=? fun pk ->
    let pkh = Signature.Public_key.hash pk in
    let animals = animals_of_pkh pkh in
    return (Animals animals)

  let pp ppf = function
    | Animals a -> Ledger_names.pp ppf a
    | Pkh pkh -> Signature.Public_key_hash.pp ppf pkh

  let to_animals =
    function
    | Animals a -> a
    | Pkh pkh -> animals_of_pkh pkh

  let equal a b = to_animals a = to_animals b
end

(** An account is a given key-pair corresponding to a
    [ledger + curve + derivation-path]. *)
module Ledger_account = struct
  type t = {
    ledger : Ledger_id.t ;
    curve : Ledgerwallet_tezos.curve ;
    path: int32 list
  }
end

(** {!Leder_uri.t} represents a parsed ["ledger://..."] URI which may
    refer to a {!Ledger_id.t} or a full blown {!Ledger_account.t}. *)
module Ledger_uri = struct

  type t = [ `Ledger of Ledger_id.t | `Ledger_account of Ledger_account.t ]

  let int32_of_path_element_exn ?(allow_weak = false) x =
    let failf ppf = Printf.ksprintf Pervasives.failwith ppf in
    let len = String.length x in
    match String.get x (len - 1) with
    | exception _ -> failf "Empty path element"
    | '\'' ->
        let intpart = String.sub x 0 (len - 1) in
        begin match Int32.of_string_opt intpart with
          | Some i -> Bip32_path.hard i
          | None -> failf "Path is not an integer: %S" intpart
        end
    | _ when allow_weak ->
        begin match Int32.of_string_opt x with
          | Some i -> i
          | None -> failf "Path is not a non-hardened integer: %S" x
        end
    | _ ->
        (* Future derivation schemes will support weak paths, not for now. *)
        failf "Non-hardened paths are not allowed (%S)" x

  let parse_animals animals =
    match String.split '-' animals with
    | [c; t; h; d] -> Some { Ledger_names.c ; t ; h ; d }
    | _ -> None

  let parse ?allow_weak uri : t tzresult Lwt.t =
    let host = Uri.host uri in
    begin match Option.apply host
                  ~f:Signature.Public_key_hash.of_b58check_opt with
    | Some pkh -> return (Ledger_id.Pkh pkh)
    | None ->
        (match Option.apply host ~f:parse_animals with
         | Some animals -> return (Ledger_id.Animals animals)
         | None ->
             failwith "Cannot parse host of URI: %s" (Uri.to_string uri))
    end >>=? fun ledger ->
    let components = String.split '/' (Uri.path uri) in
    begin match components with
      | s :: tl ->
          let curve, more_path =
            match Ledgerwallet_tezos.curve_of_string s with
            | Some curve -> curve, tl
            | None -> Ledger_id.curve, s :: tl in
          begin
            try return (List.map
                          (int32_of_path_element_exn ?allow_weak)
                          more_path)
            with Failure s ->
              failwith "Failed to parse Curve/BIP32 path from %s (%s): %s"
                (Uri.path uri) (Uri.to_string uri) s
          end
          >>=? fun bip32 ->
          return (`Ledger_account Ledger_account.{ledger; curve; path = bip32})
      | [] ->
          return (`Ledger ledger)
    end

  let ledger_uri_or_alias_param next =
    let name = "account-alias-or-ledger-uri" in
    let desc =
      "An imported ledger alias or a ledger URI \
       (e.g. \"ledger://animal/curve/path\")." in
    let open Clic in
    param ~name ~desc
      (parameter
         (fun cctxt str ->
            Public_key.find_opt cctxt str >>=? begin function
              | Some ((x : pk_uri), _) -> return (x :> Uri.t)
              | None ->
                  try return (Uri.of_string str)
                  with e ->
                    failwith "Error while parsing URI: %s"
                      (Printexc.to_string e)
            end
            >>=? fun uri ->
            parse uri))
      next

  let pp: _ -> t -> unit = fun ppf -> Format.(function
      | `Ledger lid -> fprintf ppf "ledger://%a" Ledger_id.pp lid
      | `Ledger_account {Ledger_account.ledger ; curve ; path } ->
          fprintf ppf "ledger://%a/%a/%a"
            Ledger_id.pp ledger
            Ledgerwallet_tezos.pp_curve curve
            Bip32_path.pp_path path)

  let if_matches (meta_uri : t) ledger_id cont =
    match meta_uri with
    | `Ledger l ->
        if Ledger_id.equal l ledger_id then cont () else return_none
    | `Ledger_account { Ledger_account. ledger ; _ } ->
        if Ledger_id.equal ledger ledger_id then cont () else return_none


  let full_account (ledger_uri : t) =
    begin match ledger_uri with
      | `Ledger_account acc -> return acc
      | `Ledger ledger_id ->
          failwith
            "Insufficient information: \
             you need to provide a curve & BIP32 path (%a)."
            Ledger_id.pp ledger_id
    end

end

(** Filters allow early dismissal of HID devices/ledgers which
    searching for a ledger. *)
module Filter = struct
  type version_filter =
    Ledgerwallet_tezos.Version.t * string -> bool
  type t = [
    | `None
    | `Hid_path of string
    | `Version of string * version_filter
  ]

  let version_matches (t : t) version_commit =
    match t with
    | `Version (_, f) -> f version_commit
    | _ -> true

  let is_app : _ -> _ -> t =
    fun msg app ->
      `Version (msg, fun ({Ledgerwallet_tezos.Version.app_class ; _} ,_) ->
          app = app_class)

  let is_baking = is_app "App = Baking" Ledgerwallet_tezos.Version.TezBake

  let pp ppf (f : t) =
    let open Format in
    match f with
    | `None -> fprintf ppf "None"
    | `Hid_path s -> fprintf ppf "HID-path: %s" s
    | `Version (s, _) -> fprintf ppf "%s" s
end

(* Those are always valid on Ledger Nano S with latest firmware. *)
let vendor_id = 0x2c97
let product_id = 0x0001

let use_ledger ?(filter : Filter.t = `None) f =
  let ledgers = Hidapi.enumerate ~vendor_id ~product_id () in
  log_info "Found %d Ledger(s)" (List.length ledgers) ;
  let process_device device_info f =
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
      begin match filter with
        | `Hid_path hp when device_info.path <> hp -> return_none
        | _ ->
            begin match Hidapi.(open_path device_info.path) with
              | None -> return_none
              | Some h ->
                  Lwt.finalize
                    (fun () ->
                       Ledger_commands.get_version ~device_info h
                       >>=? function
                       | Some version_git when
                           (Filter.version_matches filter version_git) ->
                           Ledger_id.get h >>=? fun ledger_id ->
                           f h version_git device_info ledger_id
                       | None | Some _ -> return_none)
                    (fun () -> Hidapi.close h ; Lwt.return_unit)
            end
      end
    else
      return_none
  in
  let rec go =
    function
    | [] -> return_none
    | h :: t ->
        process_device h f
        >>=? function
        | Some x -> return_some x
        | None -> go t
  in
  go ledgers

let use_ledger_or_fail ~ledger_uri ?filter ?msg f =
  use_ledger ?filter
    (fun hidapi (version, git_commit) device_info ledger_id ->
       Ledger_uri.if_matches ledger_uri ledger_id (fun () ->
           f hidapi (version, git_commit) device_info ledger_id))
  >>=? function
  | Some o -> return o
  | None ->
      failwith "%sFound no ledger corresponding to %a%t."
        (Option.unopt_map ~default:"" ~f:(Printf.sprintf "%s: ") msg)
        Ledger_uri.pp ledger_uri
        (fun ppf ->
           match filter with
           | Some f -> Format.fprintf ppf " with filter \"%a\"" Filter.pp f
           | None -> ())


(** A global {!Hashtbl.t} which allows us to avoid calling
    {!Signer_implementation.get_public_key} too often. *)
module Global_cache : sig
  val record :
    pk_uri ->
    pk:Signature.public_key -> pkh:Signature.public_key_hash -> unit
  val get :
    pk_uri -> (Signature.public_key_hash * Signature.public_key) option
end = struct
  let _cache :
    (pk_uri, Signature.Public_key_hash.t * Signature.Public_key.t) Hashtbl.t =
    Hashtbl.create 13

  let record pk_uri ~pk ~pkh =
    Hashtbl.replace _cache pk_uri (pkh, pk)

  let get pk_uri =
    Hashtbl.find_opt _cache pk_uri
end

(** The implementation of the “signer-plugin.” *)
module Signer_implementation : Client_keys.SIGNER = struct
  let scheme = "ledger"

  let title =
    "Built-in signer using a Ledger Nano S."

  let description =
    Printf.sprintf
      "Valid URIs are of the form\n\
      \ - ledger://<animals>/<curve>[/<path>]\n\
       where:\n\
      \ - <animals> is the identifier of the ledger of the form \
       'crouching-tiger-hidden-dragon' and can be obtained with the command \
       `tezos-client list connected ledgers` (which also provides full examples).\n\
       - <curve> is the signing curve, e.g. `ed1551`\n\
       - <path> is a BIP32 path anchored at \
       m/%s. The ledger does not yet support non-hardened paths, so each \
       node of the path must be hardened."
      Bip32_path.(string_of_path tezos_root)

  let neuterize (sk : sk_uri) = return (make_pk_uri (sk :> Uri.t))

  let pkh_of_pk = Signature.Public_key.hash

  let public_key
      ?(interactive : Client_context.io_wallet option) (pk_uri : pk_uri) =
    match Global_cache.get pk_uri with
    | Some (_, pk) -> return pk
    | None ->
        begin
          Ledger_uri.parse (pk_uri :> Uri.t) >>=? fun ledger_uri ->
          Ledger_uri.full_account ledger_uri >>=? fun { curve ; path ; _ } ->
          use_ledger_or_fail ~ledger_uri
            (fun hidapi (_version, _git_commit) _device_info _ledger_id ->
               Ledger_commands.public_key ?interactive hidapi curve path
               >>=? fun pk ->
               let pkh = pkh_of_pk pk in
               Global_cache.record pk_uri ~pkh ~pk ;
               return_some pk)
        end >>= function
        | Error err -> failwith "%a" pp_print_error err
        | Ok v -> return v

  let public_key_hash ?interactive pk_uri =
    match Global_cache.get pk_uri with
    | Some (pkh, pk) -> return (pkh, Some pk)
    | None ->
        public_key ?interactive pk_uri >>=? fun pk ->
        return (pkh_of_pk pk, Some pk)

  let sign ?watermark (sk_uri : sk_uri) msg =
    Ledger_uri.parse (sk_uri :> Uri.t) >>=? fun ledger_uri ->
    Ledger_uri.full_account ledger_uri >>=? fun { curve ; path ; _ } ->
    use_ledger_or_fail ~ledger_uri
      (fun hidapi (_version, _git_commit) _device_info _ledger_id ->
         Ledger_commands.sign ?watermark hidapi curve path msg
         >>=? fun bytes ->
         return_some bytes)

  let deterministic_nonce _ _ = fail Ledger_deterministic_nonce_not_implemented
  let deterministic_nonce_hash _ _ = fail Ledger_deterministic_nonce_not_implemented
  let supports_deterministic_nonces _ = return_false
end

(* The Ledger uses a special value 0x00000000 for the “any” chain-id: *)
let pp_ledger_chain_id fmt s =
  match s with
  | "\x00\x00\x00\x00" -> Format.fprintf fmt "'Unspecified'"
  | other -> Format.fprintf fmt "%a" Chain_id.pp (Chain_id.of_string_exn other)

(** Commands for both ledger applications. *)
let generic_commands group = Clic.[
    command ~group
      ~desc: "List supported Ledger Nano S devices connected."
      no_options
      (fixed [ "list" ; "connected" ; "ledgers" ])
      (fun () (cctxt : Client_context.full) ->
         use_ledger
           (fun _hidapi (version, git_commit) device_info ledger_id ->
              let open Hidapi in
              cctxt#message "%t"
                Format.(fun ppf ->
                    let intro =
                      asprintf
                        "Found a %a (git-description: %S) application \
                         running on %s %s at [%s]."
                        Ledgerwallet_tezos.Version.pp version
                        git_commit
                        (device_info.manufacturer_string
                         |> Option.unopt ~default:"NO-MANUFACTURER")
                        (device_info.product_string
                         |> Option.unopt ~default:"NO-PRODUCT")
                        device_info.path
                    in
                    pp_open_vbox ppf 0 ;
                    fprintf ppf "## Ledger `%a`@," Ledger_id.pp ledger_id ;
                    pp_open_hovbox ppf 0 ;
                    pp_print_text ppf intro ;
                    pp_close_box ppf () ;
                    pp_print_cut ppf () ;
                    pp_print_cut ppf () ;
                    pp_open_hovbox ppf 0 ;
                    pp_print_text ppf
                      "To use keys at BIP32 path m/44'/1729'/0'/0' \
                       (default Tezos key path), use one of:" ;
                    pp_close_box ppf () ;
                    pp_print_cut ppf () ;
                    List.iter (fun curve ->
                        fprintf ppf
                          "  tezos-client import secret key \
                           ledger_%s \"ledger://%a/%a/0'/0'\""
                          (Sys.getenv_opt "USER" |> Option.unopt ~default:"user")
                          Ledger_id.pp ledger_id
                          Ledgerwallet_tezos.pp_curve curve ;
                        pp_print_cut ppf ())
                      [ Ed25519 ; Secp256k1 ; Secp256r1 ] ;
                    pp_close_box ppf () ;
                    pp_print_newline ppf () )
              >>= fun () ->
              return_none)
         >>=? fun _ ->
         return_unit) ;
    Clic.command ~group
      ~desc: "Display version/public-key/address information for a Ledger URI"
      (args1 (switch ~doc:"Test signing operation" ~long:"test-sign" ()))
      (prefixes [ "show" ; "ledger" ]
       @@ Ledger_uri.ledger_uri_or_alias_param
       @@ stop)
      (fun test_sign ledger_uri (cctxt : Client_context.full) ->
         use_ledger_or_fail
           ~ledger_uri
           (fun hidapi (version, git_commit) device_info _ledger_id ->
              cctxt#message "Found ledger corresponding to %a:"
                Ledger_uri.pp ledger_uri
              >>= fun () ->
              cctxt#message "* Manufacturer: %s"
                (Option.unopt device_info.manufacturer_string ~default:"NONE")
              >>= fun () ->
              cctxt#message "* Product: %s"
                (Option.unopt device_info.product_string ~default:"NONE")
              >>= fun () ->
              cctxt#message "* Application: %a (git-description: %S)"
                Ledgerwallet_tezos.Version.pp version git_commit
              >>= fun () ->
              begin match ledger_uri with
                | `Ledger_account { curve ;  path ; _ } ->
                    cctxt#message "* Curve: `%a`"
                      Ledgerwallet_tezos.pp_curve curve
                    >>= fun () ->
                    let full_path = Bip32_path.tezos_root @ path in
                    cctxt#message "* Path: `%s` [%s]"
                      (Bip32_path.string_of_path full_path)
                      (String.concat "; "
                         (List.map (Printf.sprintf "0x%lX") full_path))
                    >>= fun () ->
                    Ledger_commands.public_key_hash hidapi curve path
                    >>=? fun (pkh, pk) ->
                    cctxt#message "* Public Key: %a"
                      Signature.Public_key.pp pk >>= fun () ->
                    cctxt#message "* Public Key Hash: %a@\n"
                      Signature.Public_key_hash.pp pkh >>= fun () ->
                    begin match test_sign, version.app_class with
                      | true, Tezos ->
                          let pkh_bytes =
                            Signature.Public_key_hash.to_bytes pkh in
                          (* Signing requires validation on the device.  *)
                          cctxt#message
                            "@[Attempting a signature@ (of `%a`),@ please@ \
                             validate on@ the ledger.@]"
                            MBytes.pp_hex pkh_bytes
                          >>= fun () ->
                          Ledger_commands.sign
                            ~watermark:Generic_operation
                            hidapi curve path pkh_bytes >>=? fun signature ->
                          begin match
                              Signature.check ~watermark:Generic_operation
                                pk signature pkh_bytes
                            with
                            | false ->
                                failwith "Fatal: Ledger cannot sign with %a"
                                  Signature.Public_key_hash.pp pkh
                            | true ->
                                cctxt#message "Tezos Wallet successfully signed:@ %a."
                                  Signature.pp signature
                                >>= fun () ->
                                return_unit
                          end
                      | true, TezBake ->
                          failwith "Option --test-sign only works \
                                    for the Tezos Wallet app."
                      | false, _ ->
                          return_unit
                    end
                | `Ledger _ when test_sign ->
                    failwith "Option --test-sign only works with a full \
                              ledger URI/account (with curve/path)."
                | `Ledger _ ->
                    cctxt#message "* This is just a ledger URI."
                    >>= fun () -> return_unit
              end
              >>=? fun () ->
              return_some ())) ;
  ]

(** Commands specific to the Baking app minus the high-water-mark ones
    which get a specific treatment in {!high_water_mark_commands}. *)
let baking_commands group = Clic.[
    Clic.command ~group
      ~desc: "Query the path of the authorized key"
      no_options
      (prefixes [ "get" ; "ledger" ; "authorized" ; "path" ; "for" ]
       @@ Ledger_uri.ledger_uri_or_alias_param
       @@ stop)
      (fun () ledger_uri (cctxt : Client_context.full) ->
         use_ledger_or_fail ~ledger_uri ~filter:Filter.is_baking
           (fun hidapi (version, _git_commit) _device_info _ledger_id ->
              Ledger_commands.get_authorized_path hidapi version
              >>=? fun authorized ->
              begin match authorized with
                | `Legacy_path p ->
                    cctxt#message
                      "@[<v 0>Authorized baking path (Legacy < 2.x.y): %a@]"
                      Bip32_path.pp_path p >>= fun () ->
                    return_some ()
                | `No_baking_authorized ->
                    cctxt#message "No baking key authorized at all."
                    >>= fun () ->
                    return_some ()
                | `Path_curve (ledger_path, ledger_curve) ->
                    cctxt#message
                      "@[<v 0>Authorized baking path: %a@]"
                      Bip32_path.pp_path ledger_path >>= fun () ->
                    cctxt#message
                      "@[<v 0>Authorized baking curve: %a@]"
                      Ledgerwallet_tezos.pp_curve ledger_curve >>= fun () ->
                    begin match ledger_uri with
                      | `Ledger _ -> return_some ()
                      | `Ledger_account { curve; path ; _ }
                        when curve = ledger_curve
                          && Bip32_path.tezos_root @ path = ledger_path ->
                          cctxt#message
                            "@[<v 0>Authorized baking URI: %a@]"
                            Ledger_uri.pp ledger_uri
                          >>= fun () ->
                          return_some ()
                      | `Ledger_account { curve; path ; _ } ->
                          failwith
                            "Path and curve do not match the ones \
                             specified in the command line: %a & %a"
                            Ledgerwallet_tezos.pp_curve curve
                            Bip32_path.pp_path (Bip32_path.tezos_root @ path)
                    end
              end)) ;
    Clic.command ~group
      ~desc: "Authorize a Ledger to bake for a key (deprecated, \
              use `setup ledger ...` with recent versions of the Baking app)"
      no_options
      (prefixes [ "authorize" ; "ledger" ; "to" ; "bake" ; "for" ]
       @@ Ledger_uri.ledger_uri_or_alias_param
       @@ stop)
      (fun () ledger_uri (cctxt : Client_context.full) ->
         use_ledger_or_fail ~ledger_uri ~filter:Filter.is_baking
           (fun hidapi (version, _git_commit) _device_info _ledger_id ->
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
              Ledger_uri.full_account ledger_uri
              >>=? fun { Ledger_account. curve ;  path ; _ } ->
              Ledger_commands.public_key_returning_instruction
                `Authorize_baking hidapi curve path
              >>=? fun pk ->
              let pkh = Signature.Public_key.hash pk in
              cctxt#message
                "@[<v 0>Authorized baking for address: %a@,\
                 Corresponding full public key: %a@]"
                Signature.Public_key_hash.pp pkh
                Signature.Public_key.pp pk >>= fun () ->
              return_some ())) ;
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
                 | "ASK-LEDGER" -> return_none
                 | s ->
                     try return_some (Int32.of_string s) with _ ->
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
       @@ Ledger_uri.ledger_uri_or_alias_param
       @@ stop)
      (fun (chain_id_opt, main_hwm_opt, test_hwm_opt) ledger_uri (cctxt : Client_context.full) ->
         use_ledger_or_fail ~ledger_uri ~filter:Filter.is_baking
           (fun hidapi (version, _git_commit) _device_info _ledger_id ->
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
              Ledger_uri.full_account ledger_uri
              >>=? fun { Ledger_account. curve ;  path ; _ } ->
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
              Ledger_commands.wrap_ledger_cmd begin fun pp ->
                Ledgerwallet_tezos.get_all_high_watermarks ~pp hidapi
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
              Ledger_commands.public_key_returning_instruction
                (`Setup (Chain_id.to_string main_chain_id, main_hwm, test_hwm))
                hidapi curve path
              >>=? fun pk ->
              let pkh = Signature.Public_key.hash pk in
              cctxt#message
                "@[<v 0>Authorized baking for address: %a@,\
                 Corresponding full public key: %a@]"
                Signature.Public_key_hash.pp pkh
                Signature.Public_key.pp pk >>= fun () ->
              return_some ()
           )) ;
    Clic.command ~group
      ~desc: "Deauthorize Ledger from baking"
      no_options
      (prefixes [ "deauthorize" ; "ledger" ; "baking" ; "for" ]
       @@ Ledger_uri.ledger_uri_or_alias_param
       @@ stop)
      (fun () ledger_uri (_cctxt : Client_context.full) ->
         use_ledger_or_fail ~ledger_uri ~filter:Filter.is_baking
           (fun hidapi (_version, _git_commit) _device_info _ledger_id ->
              Ledger_commands.wrap_ledger_cmd begin fun pp ->
                Ledgerwallet_tezos.deauthorize_baking ~pp hidapi
              end >>=? fun () ->
              return_some ()
           )) ;
  ]

(** Commands for high water mark of the Baking app. The
    [watermark_spelling] argument is used to make 2 sets of commands: with
    the old/wrong spelling “watermark” for backwards compatibility and
    with the correct one “high water mark” (it's a mark of the highest
    water level). *)
let high_water_mark_commands group watermark_spelling =
  let make_desc desc =
    if List.length watermark_spelling = 1
    then desc ^ " (legacy/deprecated spelling)"
    else desc in
  Clic.[
    Clic.command ~group
      ~desc:(make_desc "Get high water mark of a Ledger")
      (args1 (switch ~doc:"Prevent the fallback to the (deprecated) Ledger \
                           instructions (for 1.x.y versions of the Baking app)"
                ~long:"no-legacy-instructions" ()))
      (prefixes ([ "get" ; "ledger" ; "high" ] @ watermark_spelling @ [ "for" ])
       @@ Ledger_uri.ledger_uri_or_alias_param
       @@ stop)
      (fun no_legacy_apdu ledger_uri (cctxt : Client_context.full) ->
         use_ledger_or_fail ~ledger_uri ~filter:Filter.is_baking
           (fun hidapi (version, _git_commit) _device_info _ledger_id ->
              match version.app_class with
              | Tezos ->
                  failwith "Fatal: this operation is only valid with the \
                            Tezos Baking application"
              | TezBake when not no_legacy_apdu && version.major < 2 ->
                  Ledger_commands.wrap_ledger_cmd begin fun pp ->
                    Ledgerwallet_tezos.get_high_watermark ~pp hidapi
                  end >>=? fun hwm ->
                  cctxt#message "The high water mark for@ %a@ is %ld."
                    Ledger_uri.pp ledger_uri hwm >>= fun () ->
                  return_some ()
              | TezBake when no_legacy_apdu && version.major < 2 ->
                  failwith
                    "Cannot get the high water mark with@ \
                     `--no-legacy-instructions` and version %a"
                    Ledgerwallet_tezos.Version.pp version
              | TezBake ->
                  Ledger_commands.wrap_ledger_cmd begin fun pp ->
                    Ledgerwallet_tezos.get_all_high_watermarks ~pp hidapi
                  end
                  >>=? fun (`Main_hwm mh, `Test_hwm th, `Chain_id ci) ->
                  cctxt#message
                    "The high water mark values for@ %a@ are\
                     @ %ld for the main-chain@ (%a)@ \
                     and@ %ld for the test-chain."
                    Ledger_uri.pp ledger_uri mh pp_ledger_chain_id ci th
                  >>= fun () ->
                  return_some () ) ) ;
    Clic.command ~group
      ~desc:(make_desc "Set high water mark of a Ledger")
      no_options
      (prefixes ([ "set" ; "ledger" ; "high" ] @ watermark_spelling @ [ "for" ])
       @@ Ledger_uri.ledger_uri_or_alias_param
       @@ (prefix "to")
       @@ (param
             ~name: "high watermark"
             ~desc: "High watermark"
             (parameter (fun _ctx s ->
                  try return (Int32.of_string s)
                  with _ -> failwith "%s is not an int32 value" s)))
       @@ stop)
      (fun () ledger_uri hwm (cctxt : Client_context.full) ->
         use_ledger_or_fail ~ledger_uri ~filter:Filter.is_baking
           (fun hidapi (version, _git_commit) _device_info _ledger_id ->
              match version.app_class with
              | Tezos ->
                  failwith "Fatal: this operation is only valid with TezBake"
              | TezBake ->
                  Ledger_commands.wrap_ledger_cmd begin fun pp ->
                    Ledgerwallet_tezos.set_high_watermark ~pp hidapi hwm
                  end >>=? fun () ->
                  Ledger_commands.wrap_ledger_cmd begin fun pp ->
                    Ledgerwallet_tezos.get_high_watermark ~pp hidapi
                  end >>=? fun new_hwm ->
                  cctxt#message
                    "@[<v 0>%a has now high water mark: %ld@]"
                    Ledger_uri.pp ledger_uri new_hwm >>= fun () ->
                  return_some ())) ;
  ]

let commands =
  let group =
    { Clic.name = "ledger" ;
      title = "Commands for managing the connected Ledger Nano S devices" } in
  fun () ->
    generic_commands group @
    baking_commands group @
    high_water_mark_commands group [ "water" ; "mark" ] @
    high_water_mark_commands group [ "watermark" ]
