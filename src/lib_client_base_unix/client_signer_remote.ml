(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys
open Client_signer_remote_messages

let sign conn key data =
  let req = { Sign.Request.key = key ; data } in
  send conn Request.encoding (Request.Sign req) >>=? fun () ->
  recv conn Sign.Response.encoding >>=? function
  | Error err -> Lwt.return (Error err)
  | Ok res -> return res.signature

let public_key conn key =
  let req = { Public_key.Request.key = key } in
  send conn Request.encoding (Request.Public_key req) >>=? fun () ->
  recv conn Public_key.Response.encoding >>=? function
  | Error err -> Lwt.return (Error err)
  | Ok res -> return res.public_key


module Remote_signer : SIGNER = struct
  let scheme = "remote"

  let title =
    "Built-in signer using remote wallet."

  let description = ""

  (* secret key is the identifier of the location key identifier *)
  type secret_key = Uri.t * string
  (* public key is the key itself *)
  type public_key = Signature.Public_key.t

  let pks : (secret_key,public_key) Hashtbl.t = Hashtbl.create 53

  (* XXX : I want to reuse the connection, but this doesn't work
     let conn_table = Hashtbl.create 53
     let connect uri =
     match Hashtbl.find_opt conn_table uri with
     | None ->
        Connection.connect uri >>= fun conn ->
        Hashtbl.add conn_table uri conn;
        Lwt.return conn
     | Some conn -> Lwt.return conn
  *)

  (* load and init the remote wallet. initialize the connection *)
  let init _cctxt = return ()

  let pk_locator_of_human_input _cctxt = function
    | [] -> failwith "Remote Schema : Missing public key argument"
    | uri :: key :: _ ->
        return (
          Public_key_locator.create
            ~scheme ~location:[String.trim uri; String.trim key]
        )
    | l -> failwith
             "Remote Schema : Wrong location type %a"
             Format.(pp_print_list ~pp_sep:pp_print_cut pp_print_string) l

  let sk_to_locator (uri,key) =
    Lwt.return (
      Secret_key_locator.create
        ~scheme ~location:[Uri.to_string uri; String.trim key]
    )

  let sk_locator_of_human_input _cctxt = function
    | [] -> failwith "Remote Schema : Missing secret key argument"
    | uri_string :: key :: _ ->
        let uri = Uri.of_string uri_string in
        Connection.connect uri >>= fun conn ->
        public_key conn key >>=? fun pk ->
        Hashtbl.replace pks (uri,key) pk ;
        sk_to_locator (uri,key) >>= fun locator ->
        return locator
    | l -> failwith
             "Remote Schema : Missing secret key argument %a"
             Format.(pp_print_list ~pp_sep:pp_print_cut pp_print_string) l

  let sk_of_locator = function
    | (Sk_locator { location  = (uri :: key :: _) }) ->
        return (Uri.of_string uri, key)
    | skloc ->
        failwith "Remote Schema : sk_of_locator Wrong locator type: %s"
          (Secret_key_locator.to_string skloc)

  let pk_of_locator = function
    | (Pk_locator { location = ( location :: _ ) }) ->
        Lwt.return (Signature.Public_key.of_b58check location)
    | pkloc ->
        failwith "Remote Schema : pk_of_locator Wrong locator type: %s"
          (Public_key_locator.to_string pkloc)

  let pk_to_locator pk =
    Public_key_locator.create
      ~scheme ~location:[Signature.Public_key.to_b58check pk] |>
    Lwt.return

  let neuterize ((uri, key) as sk) =
    match Hashtbl.find_opt pks sk with
    | Some pk -> Lwt.return pk
    | None -> begin
        Connection.connect uri >>= fun conn ->
        public_key conn key >>= function
        | Error _ -> Lwt.fail_with "Remote : Cannot obtain public key from remote signer"
        | Ok pk -> begin
            Hashtbl.replace pks sk pk ;
            Lwt.return pk
          end
      end

  let public_key x = return x
  let public_key_hash x = return (Signature.Public_key.hash x)

  let sign (uri, key) msg =
    Connection.connect uri >>= fun conn ->
    sign conn key msg
end

let () =
  register_signer (module Remote_signer)
