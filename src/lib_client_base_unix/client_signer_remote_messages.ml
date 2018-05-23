(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Encoding_error
  | Decoding_error
  | Unkwnon_alias_key of string
  | Unkwnon_request_kind

let () =
  register_error_kind `Permanent
    ~id: "signer.unknown_alias_key"
    ~title: "Unkwnon_alias_key"
    ~description: "A remote key does not exists"
    ~pp: (fun ppf s ->
        Format.fprintf ppf "The key %s does not is not known on the remote signer" s)
    Data_encoding.(obj1 (req "value" string))
    (function Unkwnon_alias_key s -> Some s | _ -> None)
    (fun s -> Unkwnon_alias_key s) ;
  register_error_kind `Permanent
    ~id: "signer.unknown_request_kind"
    ~title: "Unkwnon_request_kind"
    ~description: "A request is not not understood by the remote signer"
    ~pp: (fun ppf () ->
        Format.fprintf ppf "The request is not not understood by the remote signer" )
    Data_encoding.empty
    (function Unkwnon_request_kind -> Some () | _ -> None)
    (fun () -> Unkwnon_request_kind) ;
  register_error_kind `Permanent
    ~id: "signer.encoding_error"
    ~title: "Encoding_error"
    ~description: "Error while encoding a request to the remote signer"
    ~pp: (fun ppf () ->
        Format.fprintf ppf "Could not encode a request to the remote signer")
    Data_encoding.empty
    (function Encoding_error -> Some () | _ -> None)
    (fun () -> Encoding_error) ;
  register_error_kind `Permanent
    ~id: "signer.decoding_error"
    ~title: "Decoding_error"
    ~description: "Error while decoding a request to the remote signer"
    ~pp: (fun ppf () ->
        Format.fprintf ppf "Could not decode a request to the remote signer")
    Data_encoding.empty
    (function Decoding_error -> Some () | _ -> None)
    (fun () -> Decoding_error)

type path =
  | Unix of string
  | Tcp of string * string
type key = string

module Connection = struct

  type t = Lwt_unix.file_descr

  let backlog = 10

  let read_env path =
    if path <> "" && String.get path 0 = '$' then
      try
        return (Sys.getenv (String.sub path 1 (String.length path - 1)))
      with
        Not_found ->
          match path with
          | "$TEZOS_SIGNER_TCP_HOST" -> return "localhost"
          | "$TEZOS_SIGNER_TCP_PORT" -> return "6732"
          | "$TEZOS_SIGNER_UNIX_PATH" -> return (Filename.concat (Sys.getenv "HOME") ".tezos-signer-socket")
          | _ ->
              failwith "Remote signer location uses environment variable %s which is not bound" path
    else return path

  let catch_unix_error msg f =
    Lwt.catch f @@ function
    | Unix.Unix_error (err, syscall, _) ->
        failwith "%s\nUnix error (%s): %s" msg syscall (Unix.error_message err)
    | Failure err -> failwith "%s\n%s" msg err
    | exn -> Lwt.fail exn

  let bind path =
    match path with
    | Unix path ->
        read_env path >>=? fun path ->
        catch_unix_error ("Cannot listen on " ^ path) @@ fun () ->
        let addr = Lwt_unix.ADDR_UNIX path in
        let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
        Lwt_unix.bind sock addr >>= fun () ->
        Lwt_unix.listen sock backlog ;
        return (sock, path)
    | Tcp (host, port) ->
        read_env host >>=? fun host ->
        read_env port >>=? fun port ->
        let full = host ^ ":" ^ port in
        catch_unix_error ("Cannot listen on " ^ full) @@ fun () ->
        let port = int_of_string port in
        let host = try
            (Unix.gethostbyname host).h_addr_list.(0)
          with Not_found -> Pervasives.failwith ("Host " ^ host ^ " not found") in
        let addr = Lwt_unix.ADDR_INET (host, port) in
        let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
        Lwt_unix.setsockopt sock SO_REUSEADDR true;
        Lwt_unix.bind sock addr >>= fun () ->
        Lwt_unix.listen sock backlog ;
        return (sock, full)

  let connect path =
    match path with
    | Unix path ->
        read_env path >>=? fun path ->
        let addr = Lwt_unix.ADDR_UNIX path in
        let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
        catch_unix_error ("Cannot connect to local socket " ^ path) @@ fun () ->
        Lwt_unix.connect sock addr >>= fun () ->
        return sock
    | Tcp (host, port) ->
        read_env host >>=? fun host ->
        read_env port >>=? fun port ->
        catch_unix_error ("Cannot connect to " ^ host ^ ":" ^ port) @@ fun () ->
        let port = int_of_string port in
        let host = try
            (Unix.gethostbyname host).h_addr_list.(0)
          with Not_found -> Pervasives.failwith ("Host " ^ host ^ " not found") in
        let addr = Lwt_unix.ADDR_INET (host, port) in
        let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
        Lwt_unix.connect sock addr >>= fun () ->
        return sock

  let read ~len fd buf =
    catch_unix_error "Cannot receive message" @@ fun () ->
    Lwt_utils_unix.read_mbytes ~len fd buf >>= return

  let write fd buf =
    catch_unix_error "Cannot send message" @@ fun () ->
    Lwt_utils_unix.write_mbytes fd buf >>= return

end

let message_len_size = 2

let send fd encoding message =
  let encoded_message_len =
    Data_encoding.Binary.length encoding message in
  fail_unless
    (encoded_message_len < 1 lsl (message_len_size * 8))
    Encoding_error >>=? fun () ->
  (* len is the length of int16 plus the length of the message we want to send *)
  let len = message_len_size + encoded_message_len in
  let buf = MBytes.create len in
  match Data_encoding.Binary.write
          encoding message buf message_len_size with
  | None ->
      fail Encoding_error
  | Some last ->
      fail_unless (last = len) Encoding_error >>=? fun () ->
      (* we set the beginning of the buf with the length of what is next *)
      MBytes.set_int16 buf 0 encoded_message_len ;
      Connection.write fd buf

let recv fd encoding =
  let header_buf = MBytes.create message_len_size in
  Connection.read ~len:message_len_size fd header_buf >>=? fun () ->
  let len = MBytes.get_uint16 header_buf 0 in
  let buf = MBytes.create len in
  Connection.read ~len fd buf >>=? fun () ->
  match Data_encoding.Binary.read encoding buf 0 len with
  | None ->
      fail Decoding_error
  | Some (read_len, message) ->
      if read_len <> len then
        fail Decoding_error
      else
        return message

module Sign = struct
  module Request = struct
    type t = {
      key : string ;
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
      result_encoding @@
      conv
        (fun { signature } -> (signature))
        (fun (signature)  -> { signature })
        (obj1 (req "signature" Signature.encoding))
  end
end

module Public_key = struct
  module Request = struct
    type t = {
      key : string
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
      public_key : Signature.Public_key.t
    }

    let encoding =
      let open Data_encoding in
      result_encoding @@
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
