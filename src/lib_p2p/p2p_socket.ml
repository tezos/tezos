(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO patch Data_encoding for continuation-based binary writer/reader. *)
(* TODO test `close ~wait:true`. *)
(* TODO nothing in welcoming message proves that the incoming peer is
        the owner of the public key... only the first message will
        really proves it. Should this to be changed? Not really
        important, but... an attacker might forge a random public key
        with enough proof of work (hard task), open a connection, wait
        infinitly. This would avoid the real peer to talk with us. And
        this might also have an influence on its "score". *)

include Logging.Make(struct let name = "p2p.connection" end)

type error += Decipher_error
type error += Invalid_message_size
type error += Encoding_error
type error += Rejected
type error += Decoding_error
type error += Myself of P2p_connection.Id.t
type error += Not_enough_proof_of_work of P2p_peer.Id.t
type error += Invalid_auth
type error += Invalid_chunks_size of { value: int ; min: int ; max: int }

module Crypto = struct

  let bufsize = 1 lsl 16 - 1
  let header_length = 2
  let max_content_length = bufsize - header_length - Crypto_box.boxzerobytes

  type data = {
    channel_key : Crypto_box.channel_key ;
    mutable local_nonce : Crypto_box.nonce ;
    mutable remote_nonce : Crypto_box.nonce ;
  }

  let write_chunk fd cryptobox_data msg =
    let msglen = MBytes.length msg in
    fail_unless
      (msglen <= max_content_length) Invalid_message_size >>=? fun () ->
    let buf = MBytes.init (msglen + Crypto_box.zerobytes) '\x00' in
    MBytes.blit msg 0 buf Crypto_box.zerobytes msglen ;
    let local_nonce = cryptobox_data.local_nonce in
    cryptobox_data.local_nonce <- Crypto_box.increment_nonce local_nonce ;
    Crypto_box.fast_box_noalloc
      cryptobox_data.channel_key local_nonce buf ;
    let encrypted_length = msglen + Crypto_box.boxzerobytes in
    MBytes.set_int16 buf
      (Crypto_box.boxzerobytes - header_length) encrypted_length ;
    let payload = MBytes.sub buf (Crypto_box.boxzerobytes - header_length)
        (header_length + encrypted_length) in
    P2p_io_scheduler.write fd payload

  let read_chunk fd cryptobox_data =
    let header_buf = MBytes.create header_length in
    P2p_io_scheduler.read_full ~len:header_length fd header_buf >>=? fun () ->
    let encrypted_length = MBytes.get_uint16 header_buf 0 in
    let buf = MBytes.init (encrypted_length + Crypto_box.boxzerobytes) '\x00' in
    P2p_io_scheduler.read_full
      ~pos:Crypto_box.boxzerobytes ~len:encrypted_length fd buf >>=? fun () ->
    let remote_nonce = cryptobox_data.remote_nonce in
    cryptobox_data.remote_nonce <- Crypto_box.increment_nonce remote_nonce ;
    match
      Crypto_box.fast_box_open_noalloc
        cryptobox_data.channel_key remote_nonce buf
    with
    | false ->
        fail Decipher_error
    | true ->
        return (MBytes.sub buf Crypto_box.zerobytes
                  (encrypted_length - Crypto_box.boxzerobytes))

end

let check_binary_chunks_size  size =
  let value = size - Crypto_box.boxzerobytes - Crypto.header_length in
  fail_unless
    (value > 0 &&
     value <= Crypto.max_content_length)
    (Invalid_chunks_size
       { value = size ;
         min = Crypto.(header_length + Crypto_box.boxzerobytes + 1) ;
         max = Crypto.bufsize ;
       })

module Connection_message = struct

  type t = {
    port : int option ;
    versions : P2p_version.t list ;
    public_key : Crypto_box.public_key ;
    proof_of_work_stamp : Crypto_box.nonce ;
    message_nonce : Crypto_box.nonce ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { port ; public_key ; proof_of_work_stamp ;
             message_nonce ; versions } ->
        let port = match port with None -> 0 | Some port -> port in
        (port, public_key, proof_of_work_stamp,
         message_nonce, versions))
      (fun (port, public_key, proof_of_work_stamp,
            message_nonce, versions) ->
        let port = if port = 0 then None else Some port in
        { port ; public_key ; proof_of_work_stamp ;
          message_nonce ; versions })
      (obj5
         (req "port" uint16)
         (req "pubkey" Crypto_box.public_key_encoding)
         (req "proof_of_work_stamp" Crypto_box.nonce_encoding)
         (req "message_nonce" Crypto_box.nonce_encoding)
         (req "versions" (Variable.list P2p_version.encoding)))

  let write fd message =
    let encoded_message_len =
      Data_encoding.Binary.length encoding message in
    fail_unless
      (encoded_message_len < 1 lsl (Crypto.header_length * 8))
      Encoding_error >>=? fun () ->
    let len = Crypto.header_length + encoded_message_len in
    let buf = MBytes.create len in
    match Data_encoding.Binary.write
            encoding message buf Crypto.header_length with
    | None ->
        fail Encoding_error
    | Some last ->
        fail_unless (last = len) Encoding_error >>=? fun () ->
        MBytes.set_int16 buf 0 encoded_message_len ;
        P2p_io_scheduler.write fd buf

  let read fd =
    let header_buf = MBytes.create Crypto.header_length in
    P2p_io_scheduler.read_full
      ~len:Crypto.header_length fd header_buf >>=? fun () ->
    let len = MBytes.get_uint16 header_buf 0 in
    let buf = MBytes.create len in
    P2p_io_scheduler.read_full ~len fd buf >>=? fun () ->
    match Data_encoding.Binary.read encoding buf 0 len with
    | None ->
        fail Decoding_error
    | Some (read_len, message) ->
        if read_len <> len then
          fail Decoding_error
        else
          return message

end

module Ack = struct

  type t = Ack | Nack
  let ack = MBytes.of_string "\255"
  let nack = MBytes.of_string "\000"

  let write cryptobox_data fd b =
    Crypto.write_chunk cryptobox_data fd
      (match b with Ack -> ack | Nack -> nack)

  let read fd cryptobox_data =
    Crypto.read_chunk fd cryptobox_data >>=? fun buf ->
    return (buf <> nack)

end

type authenticated_fd =
  P2p_io_scheduler.connection * P2p_connection.Info.t * Crypto.data

let kick (fd, _ , cryptobox_data) =
  Ack.write fd cryptobox_data Nack >>= fun _ ->
  P2p_io_scheduler.close fd >>= fun _ ->
  Lwt.return_unit

(* First step: write and read credentials, makes no difference
   whether we're trying to connect to a peer or checking an incoming
   connection, both parties must first introduce themselves. *)
let authenticate
    ~proof_of_work_target
    ~incoming fd (remote_addr, remote_socket_port as point)
    ?listening_port identity supported_versions =
  let local_nonce = Crypto_box.random_nonce () in
  lwt_debug "Sending authenfication to %a" P2p_point.Id.pp point >>= fun () ->
  Connection_message.write fd
    { public_key = identity.P2p_identity.public_key ;
      proof_of_work_stamp = identity.proof_of_work_stamp ;
      message_nonce = local_nonce ;
      port = listening_port ;
      versions = supported_versions } >>=? fun () ->
  Connection_message.read fd >>=? fun msg ->
  let remote_listening_port =
    if incoming then msg.port else Some remote_socket_port in
  let id_point = remote_addr, remote_listening_port in
  let remote_peer_id = Crypto_box.hash msg.public_key in
  fail_unless
    (remote_peer_id <> identity.P2p_identity.peer_id)
    (Myself id_point) >>=? fun () ->
  fail_unless
    (Crypto_box.check_proof_of_work
       msg.public_key msg.proof_of_work_stamp proof_of_work_target)
    (Not_enough_proof_of_work remote_peer_id) >>=? fun () ->
  let channel_key =
    Crypto_box.precompute identity.P2p_identity.secret_key msg.public_key in
  let info =
    { P2p_connection.Info.peer_id = remote_peer_id ;
      versions = msg.versions ; incoming ;
      id_point ; remote_socket_port ;} in
  let cryptobox_data =
    { Crypto.channel_key ; local_nonce ;
      remote_nonce = msg.message_nonce } in
  return (info, (fd, info, cryptobox_data))

type connection = {
  id : int ;
  info : P2p_connection.Info.t ;
  fd : P2p_io_scheduler.connection ;
  cryptobox_data : Crypto.data ;
}

let next_conn_id =
  let cpt = ref 0 in
  fun () -> incr cpt ;!cpt

module Reader = struct

  type 'msg t = {
    canceler: Lwt_canceler.t ;
    conn: connection ;
    encoding: 'msg Data_encoding.t ;
    messages: (int * 'msg) tzresult Lwt_pipe.t ;
    mutable worker: unit Lwt.t ;
  }

  let read_message st init_mbytes =
    let rec loop status =
      Lwt_unix.yield () >>= fun () ->
      let open Data_encoding.Binary in
      match status with
      | Success { res ; res_len ; remaining } ->
          return (Some (res, res_len, remaining))
      | Error ->
          lwt_debug "[read_message] incremental decoding error" >>= fun () ->
          return None
      | Await decode_next_buf ->
          Lwt_utils_unix.protect ~canceler:st.canceler begin fun () ->
            Crypto.read_chunk st.conn.fd st.conn.cryptobox_data
          end >>=? fun buf ->
          lwt_debug
            "reading %d bytes from %a"
            (MBytes.length buf) P2p_connection.Info.pp st.conn.info >>= fun () ->
          loop (decode_next_buf buf) in
    loop
      (Data_encoding.Binary.read_stream_of_bytes ~init:init_mbytes st.encoding)


  let rec worker_loop st init_mbytes =
    begin
      read_message st init_mbytes >>=? fun msg ->
      match msg with
      | None ->
          Lwt_utils_unix.protect ~canceler:st.canceler begin fun () ->
            Lwt_pipe.push st.messages (Error [Decoding_error]) >>= fun () ->
            return None
          end
      | Some (msg, size, rem_mbytes) ->
          Lwt_utils_unix.protect ~canceler:st.canceler begin fun () ->
            Lwt_pipe.push st.messages (Ok (size, msg)) >>= fun () ->
            return (Some rem_mbytes)
          end
    end >>= function
    | Ok Some rem_mbytes ->
        worker_loop st rem_mbytes
    | Ok None ->
        Lwt_canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit
    | Error [Lwt_utils_unix.Canceled | Exn Lwt_pipe.Closed] ->
        lwt_debug "connection closed to %a"
          P2p_connection.Info.pp st.conn.info >>= fun () ->
        Lwt.return_unit
    | Error _ as err ->
        Lwt_pipe.safe_push_now st.messages err ;
        Lwt_canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit

  let run ?size conn encoding canceler =
    let compute_size = function
      | Ok (size, _) -> (Sys.word_size / 8) * 11 + size + Lwt_pipe.push_overhead
      | Error _ -> 0 (* we push Error only when we close the socket,
                        we don't fear memory leaks in that case... *) in
    let size = Option.map size ~f:(fun max -> (max, compute_size)) in
    let st =
      { canceler ; conn ; encoding ;
        messages = Lwt_pipe.create ?size () ;
        worker = Lwt.return_unit ;
      } in
    Lwt_canceler.on_cancel st.canceler begin fun () ->
      Lwt_pipe.close st.messages ;
      Lwt.return_unit
    end ;
    st.worker <-
      Lwt_utils.worker "reader"
        ~run:(fun () -> worker_loop st [])
        ~cancel:(fun () -> Lwt_canceler.cancel st.canceler) ;
    st

  let shutdown st =
    Lwt_canceler.cancel st.canceler >>= fun () ->
    st.worker

end

module Writer = struct

  type 'msg t = {
    canceler: Lwt_canceler.t ;
    conn: connection ;
    encoding: 'msg Data_encoding.t ;
    messages: (MBytes.t list * unit tzresult Lwt.u option) Lwt_pipe.t ;
    mutable worker: unit Lwt.t ;
    binary_chunks_size: int ; (* in bytes *)
  }

  let send_message st buf =
    let rec loop = function
      | [] -> return ()
      | buf :: l ->
          Lwt_utils_unix.protect ~canceler:st.canceler begin fun () ->
            Crypto.write_chunk st.conn.fd st.conn.cryptobox_data buf
          end >>=? fun () ->
          lwt_debug "writing %d bytes to %a"
            (MBytes.length buf) P2p_connection.Info.pp st.conn.info >>= fun () ->
          loop l in
    loop buf

  let encode_message st msg =
    try ok (Data_encoding.Binary.to_bytes_list st.binary_chunks_size st.encoding msg)
    with _ -> error Encoding_error

  let rec worker_loop st =
    Lwt_unix.yield () >>= fun () ->
    Lwt_utils_unix.protect ~canceler:st.canceler begin fun () ->
      Lwt_pipe.pop st.messages >>= return
    end >>= function
    | Error [Lwt_utils_unix.Canceled | Exn Lwt_pipe.Closed] ->
        lwt_debug "connection closed to %a"
          P2p_connection.Info.pp st.conn.info >>= fun () ->
        Lwt.return_unit
    | Error err ->
        lwt_log_error
          "@[<v 2>error writing to %a@ %a@]"
          P2p_connection.Info.pp st.conn.info pp_print_error err >>= fun () ->
        Lwt_canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit
    | Ok (buf, wakener) ->
        send_message st buf >>= fun res ->
        match res with
        | Ok () ->
            Option.iter wakener ~f:(fun u -> Lwt.wakeup_later u res) ;
            worker_loop st
        | Error err ->
            Option.iter wakener
              ~f:(fun u ->
                  Lwt.wakeup_later u
                    (Error [P2p_io_scheduler.Connection_closed])) ;
            match err with
            | [ Lwt_utils_unix.Canceled | Exn Lwt_pipe.Closed ] ->
                lwt_debug "connection closed to %a"
                  P2p_connection.Info.pp st.conn.info >>= fun () ->
                Lwt.return_unit
            | [ P2p_io_scheduler.Connection_closed ] ->
                lwt_debug "connection closed to %a"
                  P2p_connection.Info.pp st.conn.info >>= fun () ->
                Lwt_canceler.cancel st.canceler >>= fun () ->
                Lwt.return_unit
            | err ->
                lwt_log_error
                  "@[<v 2>error writing to %a@ %a@]"
                  P2p_connection.Info.pp st.conn.info
                  pp_print_error err >>= fun () ->
                Lwt_canceler.cancel st.canceler >>= fun () ->
                Lwt.return_unit

  let run
      ?size ?binary_chunks_size
      conn encoding canceler =
    let binary_chunks_size =
      match binary_chunks_size with
      | None -> Crypto.max_content_length
      | Some size ->
          let size = size - Crypto_box.boxzerobytes - Crypto.header_length in
          assert (size > 0) ;
          assert (size <= Crypto.max_content_length) ;
          size
    in
    let compute_size =
      let buf_list_size =
        List.fold_left
          (fun sz buf ->
             sz + MBytes.length buf + 2 * Sys.word_size) 0
      in
      function
      | buf_l, None ->
          Sys.word_size + buf_list_size buf_l + Lwt_pipe.push_overhead
      | buf_l, Some _ ->
          2 * Sys.word_size + buf_list_size buf_l + Lwt_pipe.push_overhead
    in
    let size = Option.map size ~f:(fun max -> max, compute_size) in
    let st =
      { canceler ; conn ; encoding ;
        messages = Lwt_pipe.create ?size () ;
        worker = Lwt.return_unit ;
        binary_chunks_size = binary_chunks_size ;
      } in
    Lwt_canceler.on_cancel st.canceler begin fun () ->
      Lwt_pipe.close st.messages ;
      while not (Lwt_pipe.is_empty st.messages) do
        let _, w = Lwt_pipe.pop_now_exn st.messages in
        Option.iter w
          ~f:(fun u -> Lwt.wakeup_later u (Error [Exn Lwt_pipe.Closed]))
      done ;
      Lwt.return_unit
    end ;
    st.worker <-
      Lwt_utils.worker "writer"
        ~run:(fun () -> worker_loop st)
        ~cancel:(fun () -> Lwt_canceler.cancel st.canceler) ;
    st

  let shutdown st =
    Lwt_canceler.cancel st.canceler >>= fun () ->
    st.worker

end

type 'msg t = {
  conn : connection ;
  reader : 'msg Reader.t ;
  writer : 'msg Writer.t ;
}

let equal { conn = { id = id1 } } { conn = { id = id2 } } = id1 = id2

let pp ppf { conn } = P2p_connection.Info.pp ppf conn.info
let info { conn } = conn.info

let accept
    ?incoming_message_queue_size ?outgoing_message_queue_size
    ?binary_chunks_size (fd, info, cryptobox_data) encoding =
  Lwt_utils_unix.protect begin fun () ->
    Ack.write fd cryptobox_data Ack >>=? fun () ->
    Ack.read fd cryptobox_data
  end ~on_error:begin fun err ->
    P2p_io_scheduler.close fd >>= fun _ ->
    match err with
    | [ P2p_io_scheduler.Connection_closed ] -> fail Rejected
    | [ Decipher_error ] -> fail Invalid_auth
    | err -> Lwt.return (Error err)
  end >>=? fun accepted ->
  fail_unless accepted Rejected >>=? fun () ->
  let canceler = Lwt_canceler.create () in
  let conn = { id = next_conn_id () ; fd ; info ; cryptobox_data } in
  let reader =
    Reader.run ?size:incoming_message_queue_size conn encoding canceler
  and writer =
    Writer.run
      ?size:outgoing_message_queue_size ?binary_chunks_size
      conn encoding canceler
  in
  let conn = { conn ; reader ; writer } in
  Lwt_canceler.on_cancel canceler begin fun () ->
    P2p_io_scheduler.close fd >>= fun _ ->
    Lwt.return_unit
  end ;
  return conn

let catch_closed_pipe f =
  Lwt.catch f begin function
    | Lwt_pipe.Closed -> fail P2p_io_scheduler.Connection_closed
    | exn -> fail (Exn exn)
  end >>= function
  | Error [Exn Lwt_pipe.Closed] ->
      fail P2p_io_scheduler.Connection_closed
  | Error _ | Ok _ as v -> Lwt.return v

let pp_json encoding ppf msg =
  Format.pp_print_string ppf
    (Data_encoding_ezjsonm.to_string
       (Data_encoding.Json.construct encoding msg))

let write { writer ; conn } msg =
  catch_closed_pipe begin fun () ->
    debug "Sending message to %a: %a"
      P2p_peer.Id.pp_short conn.info.peer_id (pp_json writer.encoding) msg ;
    Lwt.return (Writer.encode_message writer msg) >>=? fun buf ->
    Lwt_pipe.push writer.messages (buf, None) >>= return
  end

let write_sync { writer ; conn } msg =
  catch_closed_pipe begin fun () ->
    let waiter, wakener = Lwt.wait () in
    debug "Sending message to %a: %a"
      P2p_peer.Id.pp_short conn.info.peer_id (pp_json writer.encoding) msg ;
    Lwt.return (Writer.encode_message writer msg) >>=? fun buf ->
    Lwt_pipe.push writer.messages (buf, Some wakener) >>= fun () ->
    waiter
  end

let write_now { writer ; conn } msg =
  debug "Try sending message to %a: %a"
    P2p_peer.Id.pp_short conn.info.peer_id (pp_json writer.encoding) msg ;
  Writer.encode_message writer msg >>? fun buf ->
  try Ok (Lwt_pipe.push_now writer.messages (buf, None))
  with Lwt_pipe.Closed -> Error [P2p_io_scheduler.Connection_closed]

let rec split_bytes size bytes =
  if MBytes.length bytes <= size then
    [bytes]
  else
    MBytes.sub bytes 0 size ::
    split_bytes size (MBytes.sub bytes size (MBytes.length bytes - size))

let raw_write_sync { writer } bytes =
  let bytes = split_bytes writer.binary_chunks_size bytes in
  catch_closed_pipe begin fun () ->
    let waiter, wakener = Lwt.wait () in
    Lwt_pipe.push writer.messages (bytes, Some wakener) >>= fun () ->
    waiter
  end

let is_readable { reader } =
  not (Lwt_pipe.is_empty reader.messages)
let wait_readable { reader } =
  catch_closed_pipe begin fun () ->
    Lwt_pipe.values_available reader.messages >>= return
  end
let read { reader } =
  catch_closed_pipe begin fun () ->
    Lwt_pipe.pop reader.messages
  end
let read_now { reader } =
  try Lwt_pipe.pop_now reader.messages
  with Lwt_pipe.Closed -> Some (Error [P2p_io_scheduler.Connection_closed])

let stat { conn = { fd } } = P2p_io_scheduler.stat fd

let close ?(wait = false) st =
  begin
    if not wait then Lwt.return_unit
    else begin
      Lwt_pipe.close st.reader.messages ;
      Lwt_pipe.close st.writer.messages ;
      st.writer.worker
    end
  end >>= fun () ->
  Reader.shutdown st.reader >>= fun () ->
  Writer.shutdown st.writer >>= fun () ->
  P2p_io_scheduler.close st.conn.fd >>= fun _ ->
  Lwt.return_unit

