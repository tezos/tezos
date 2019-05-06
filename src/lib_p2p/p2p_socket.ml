(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* TODO test `close ~wait:true`. *)

include Internal_event.Legacy_logging.Make(struct let name = "p2p.connection" end)

module Crypto = struct

  (* maximal size of the buffer *)
  let bufsize = 1 lsl 16 - 1
  let header_length = 2
  let max_content_length = bufsize - Crypto_box.zerobytes

  (* The size of extra data added by encryption. *)
  let boxextrabytes = Crypto_box.zerobytes - Crypto_box.boxzerobytes
  (* The number of bytes added by encryption + header *)
  let extrabytes = header_length + boxextrabytes

  type data = {
    channel_key : Crypto_box.channel_key ;
    mutable local_nonce : Crypto_box.nonce ;
    mutable remote_nonce : Crypto_box.nonce ;
  }

  (* We do the following assumptions on the NaCl library.  Note that
     we also make the assumption, here, that the NaCl library allows
     in-place boxing and unboxing, since we use the same buffer for
     input and output. *)
  let () = assert (Crypto_box.boxzerobytes >= header_length)

  let write_chunk ?canceler fd cryptobox_data msg =
    let msglen = MBytes.length msg in
    fail_unless
      (msglen <= max_content_length) P2p_errors.Invalid_message_size >>=? fun () ->
    let buf_length = msglen + Crypto_box.zerobytes in
    let buf = MBytes.make buf_length '\x00' in
    MBytes.blit msg 0 buf Crypto_box.zerobytes msglen ;
    let local_nonce = cryptobox_data.local_nonce in
    cryptobox_data.local_nonce <- Crypto_box.increment_nonce local_nonce ;
    Crypto_box.fast_box_noalloc
      cryptobox_data.channel_key local_nonce buf ;
    let encrypted_length = buf_length - Crypto_box.boxzerobytes in
    let header_pos = Crypto_box.boxzerobytes - header_length in
    MBytes.set_int16 buf header_pos encrypted_length ;
    let payload = MBytes.sub buf header_pos (buf_length - header_pos) in
    P2p_io_scheduler.write ?canceler fd payload

  let read_chunk ?canceler fd cryptobox_data =
    let header_buf = MBytes.create header_length in
    P2p_io_scheduler.read_full ?canceler ~len:header_length fd header_buf >>=? fun () ->
    let encrypted_length = MBytes.get_uint16 header_buf 0 in
    let buf_length = encrypted_length + Crypto_box.boxzerobytes in
    let buf = MBytes.make buf_length '\x00' in
    P2p_io_scheduler.read_full ?canceler
      ~pos:Crypto_box.boxzerobytes ~len:encrypted_length fd buf >>=? fun () ->
    let remote_nonce = cryptobox_data.remote_nonce in
    cryptobox_data.remote_nonce <- Crypto_box.increment_nonce remote_nonce ;
    match
      Crypto_box.fast_box_open_noalloc
        cryptobox_data.channel_key remote_nonce buf
    with
    | false ->
        fail P2p_errors.Decipher_error
    | true ->
        return (MBytes.sub buf Crypto_box.zerobytes
                  (buf_length - Crypto_box.zerobytes))

end

(* Note: there is an inconsistency here, since we display an error in
   bytes, whereas the option is set in kbytes. Also, since the default
   size is 64kB-1, it is actually impossible to set the default
   size using the option (the max is 63 kB). *)
let check_binary_chunks_size size =
  let value = size - Crypto.extrabytes in
  fail_unless
    (value > 0 &&
     value <= Crypto.max_content_length)
    (P2p_errors.Invalid_chunks_size
       { value = size ;
         min = Crypto.extrabytes + 1 ;
         max = Crypto.bufsize ;
       })

module Connection_message = struct

  type t = {
    port : int option ;
    public_key : Crypto_box.public_key ;
    proof_of_work_stamp : Crypto_box.nonce ;
    message_nonce : Crypto_box.nonce ;
    version : Network_version.t ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { port ; public_key ; proof_of_work_stamp ;
             message_nonce ; version } ->
        let port = match port with None -> 0 | Some port -> port in
        (port, public_key, proof_of_work_stamp,
         message_nonce, version))
      (fun (port, public_key, proof_of_work_stamp,
            message_nonce, version) ->
        let port = if port = 0 then None else Some port in
        { port ; public_key ; proof_of_work_stamp ;
          message_nonce ; version })
      (obj5
         (req "port" uint16)
         (req "pubkey" Crypto_box.public_key_encoding)
         (req "proof_of_work_stamp" Crypto_box.nonce_encoding)
         (req "message_nonce" Crypto_box.nonce_encoding)
         (req "version" Network_version.encoding))

  let write ~canceler fd message =
    let encoded_message_len =
      Data_encoding.Binary.length encoding message in
    fail_unless
      (encoded_message_len < 1 lsl (Crypto.header_length * 8))
      P2p_errors.Encoding_error >>=? fun () ->
    let len = Crypto.header_length + encoded_message_len in
    let buf = MBytes.create len in
    match Data_encoding.Binary.write
            encoding message buf Crypto.header_length len with
    | None ->
        fail P2p_errors.Encoding_error
    | Some last ->
        fail_unless (last = len) P2p_errors.Encoding_error >>=? fun () ->
        MBytes.set_int16 buf 0 encoded_message_len ;
        P2p_io_scheduler.write ~canceler fd buf >>=? fun () ->
        (* We return the raw message as it is used later to compute
           the nonces *)
        return buf

  let read ~canceler fd =
    let header_buf = MBytes.create Crypto.header_length in
    P2p_io_scheduler.read_full ~canceler
      ~len:Crypto.header_length fd header_buf >>=? fun () ->
    let len = MBytes.get_uint16 header_buf 0 in
    let pos = Crypto.header_length in
    let buf = MBytes.create (pos + len) in
    MBytes.set_int16 buf 0 len ;
    P2p_io_scheduler.read_full ~canceler ~len ~pos fd buf >>=? fun () ->
    match Data_encoding.Binary.read encoding buf pos len with
    | None ->
        fail P2p_errors.Decoding_error
    | Some (next_pos, message) ->
        if next_pos <> pos+len then
          fail P2p_errors.Decoding_error
        else
          return (message, buf)

end

type 'meta metadata_config = {
  conn_meta_encoding : 'meta Data_encoding.t ;
  conn_meta_value : P2p_peer.Id.t -> 'meta ;
  private_node : 'meta -> bool ;
}

module Metadata = struct

  let write ~canceler metadata_config cryptobox_data fd message =
    let encoded_message_len =
      Data_encoding.Binary.length metadata_config.conn_meta_encoding message in
    let buf = MBytes.create encoded_message_len in
    match
      Data_encoding.Binary.write
        metadata_config.conn_meta_encoding message buf 0 encoded_message_len
    with
    | None ->
        fail P2p_errors.Encoding_error
    | Some last ->
        fail_unless (last = encoded_message_len)
          P2p_errors.Encoding_error >>=? fun () ->
        Crypto.write_chunk ~canceler cryptobox_data fd buf

  let read ~canceler metadata_config fd cryptobox_data =
    Crypto.read_chunk ~canceler fd cryptobox_data >>=? fun buf ->
    let length = MBytes.length buf in
    let encoding = metadata_config.conn_meta_encoding in
    match
      Data_encoding.Binary.read encoding buf 0 length
    with
    | None ->
        fail P2p_errors.Decoding_error
    | Some (read_len, message) ->
        if read_len <> length then
          fail P2p_errors.Decoding_error
        else
          return message

end

module Ack = struct

  type t = Ack | Nack

  let encoding =
    let open Data_encoding in
    let ack_encoding = obj1 (req "ack" empty) in
    let nack_encoding = obj1 (req "nack" empty) in
    let ack_case tag =
      case tag ack_encoding
        ~title:"Ack"
        (function
          | Ack -> Some ()
          | _ -> None)
        (fun () -> Ack) in
    let nack_case tag =
      case tag nack_encoding
        ~title:"Nack"
        (function
          | Nack -> Some ()
          | _ -> None
        )
        (fun _ -> Nack) in
    union [
      ack_case (Tag 0) ;
      nack_case (Tag 255) ;
    ]

  let write ?canceler fd cryptobox_data message =
    let encoded_message_len =
      Data_encoding.Binary.length encoding message in
    let buf = MBytes.create encoded_message_len in
    match Data_encoding.Binary.write encoding message buf 0 encoded_message_len with
    | None ->
        fail P2p_errors.Encoding_error
    | Some last ->
        fail_unless (last = encoded_message_len)
          P2p_errors.Encoding_error >>=? fun () ->
        Crypto.write_chunk ?canceler fd cryptobox_data buf

  let read ?canceler fd cryptobox_data =
    Crypto.read_chunk ?canceler fd cryptobox_data >>=? fun buf ->
    let length = MBytes.length buf in
    match Data_encoding.Binary.read encoding buf 0 length with
    | None ->
        fail P2p_errors.Decoding_error
    | Some (read_len, message) ->
        if read_len <> length then
          fail P2p_errors.Decoding_error
        else
          return message

end

type 'meta authenticated_connection = {
  fd: P2p_io_scheduler.connection ;
  info: 'meta P2p_connection.Info.t ;
  cryptobox_data: Crypto.data ;
}

let kick { fd ; cryptobox_data ; _ } =
  Ack.write fd cryptobox_data Nack >>= fun _ ->
  P2p_io_scheduler.close fd >>= fun _ ->
  Lwt.return_unit

(* First step: write and read credentials, makes no difference
   whether we're trying to connect to a peer or checking an incoming
   connection, both parties must first introduce themselves. *)
let authenticate
    ~canceler
    ~proof_of_work_target
    ~incoming fd (remote_addr, remote_socket_port as point)
    ?listening_port identity announced_version metadata_config =
  let local_nonce_seed = Crypto_box.random_nonce () in
  lwt_debug "Sending authenfication to %a" P2p_point.Id.pp point >>= fun () ->
  Connection_message.write ~canceler fd
    { public_key = identity.P2p_identity.public_key ;
      proof_of_work_stamp = identity.proof_of_work_stamp ;
      message_nonce = local_nonce_seed ;
      port = listening_port ;
      version = announced_version } >>=? fun sent_msg ->
  Connection_message.read ~canceler fd >>=? fun (msg, recv_msg) ->
  let remote_listening_port =
    if incoming then msg.port else Some remote_socket_port in
  let id_point = remote_addr, remote_listening_port in
  let remote_peer_id = Crypto_box.hash msg.public_key in
  fail_unless
    (remote_peer_id <> identity.P2p_identity.peer_id)
    (P2p_errors.Myself id_point) >>=? fun () ->
  fail_unless
    (Crypto_box.check_proof_of_work
       msg.public_key msg.proof_of_work_stamp proof_of_work_target)
    (P2p_errors.Not_enough_proof_of_work remote_peer_id) >>=? fun () ->
  let channel_key =
    Crypto_box.precompute identity.P2p_identity.secret_key msg.public_key in
  let (local_nonce, remote_nonce) =
    Crypto_box.generate_nonces ~incoming ~sent_msg ~recv_msg in
  let cryptobox_data = { Crypto.channel_key ; local_nonce ; remote_nonce } in
  let local_metadata = metadata_config.conn_meta_value remote_peer_id in
  Metadata.write ~canceler metadata_config fd cryptobox_data local_metadata >>=? fun () ->
  Metadata.read ~canceler metadata_config fd cryptobox_data >>=? fun remote_metadata ->
  let info =
    { P2p_connection.Info.peer_id = remote_peer_id ;
      announced_version = msg.version ; incoming ;
      id_point ; remote_socket_port ;
      private_node = metadata_config.private_node remote_metadata ;
      local_metadata ;
      remote_metadata ;
    } in
  return (info, { fd ; info ; cryptobox_data })

module Reader = struct

  type ('msg, 'meta) t = {
    canceler: Lwt_canceler.t ;
    conn: 'meta authenticated_connection ;
    encoding: 'msg Data_encoding.t ;
    messages: (int * 'msg) tzresult Lwt_pipe.t ;
    mutable worker: unit Lwt.t ;
  }

  let read_message st init =
    let rec loop status =
      Lwt_unix.yield () >>= fun () ->
      let open Data_encoding.Binary in
      match status with
      | Success { result ; size ; stream } ->
          return_some (result, size, stream)
      | Error _err ->
          lwt_debug "[read_message] incremental decoding error" >>= fun () ->
          return_none
      | Await decode_next_buf ->
          Crypto.read_chunk ~canceler:st.canceler
            st.conn.fd st.conn.cryptobox_data >>=? fun buf ->
          lwt_debug
            "reading %d bytes from %a"
            (MBytes.length buf) P2p_peer.Id.pp st.conn.info.peer_id >>= fun () ->
          loop (decode_next_buf buf) in
    loop (Data_encoding.Binary.read_stream ?init st.encoding)


  let rec worker_loop st stream =
    begin
      read_message st stream >>=? fun msg ->
      match msg with
      | None ->
          protect ~canceler:st.canceler begin fun () ->
            Lwt_pipe.push st.messages (Error [P2p_errors.Decoding_error]) >>= fun () ->
            return_none
          end
      | Some (msg, size, stream) ->
          protect ~canceler:st.canceler begin fun () ->
            Lwt_pipe.push st.messages (Ok (size, msg)) >>= fun () ->
            return_some stream
          end
    end >>= function
    | Ok (Some stream) ->
        worker_loop st (Some stream)
    | Ok None ->
        Lwt_canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit
    | Error [Canceled | Exn Lwt_pipe.Closed] ->
        lwt_debug "connection closed to %a"
          P2p_peer.Id.pp st.conn.info.peer_id >>= fun () ->
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
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop st None)
        ~cancel:(fun () -> Lwt_canceler.cancel st.canceler) ;
    st

  let shutdown st =
    Lwt_canceler.cancel st.canceler >>= fun () ->
    st.worker

end

module Writer = struct

  type ('msg, 'meta) t = {
    canceler: Lwt_canceler.t ;
    conn: 'meta authenticated_connection ;
    encoding: 'msg Data_encoding.t ;
    messages: (MBytes.t list * unit tzresult Lwt.u option) Lwt_pipe.t ;
    mutable worker: unit Lwt.t ;
    binary_chunks_size: int ; (* in bytes *)
  }

  let send_message st buf =
    let rec loop = function
      | [] -> return_unit
      | buf :: l ->
          Crypto.write_chunk ~canceler:st.canceler
            st.conn.fd st.conn.cryptobox_data buf >>=? fun () ->
          lwt_debug "writing %d bytes to %a"
            (MBytes.length buf) P2p_peer.Id.pp st.conn.info.peer_id >>= fun () ->
          loop l in
    loop buf

  let encode_message st msg =
    try ok (MBytes.cut
              st.binary_chunks_size
              (Data_encoding.Binary.to_bytes_exn st.encoding msg))
    with Data_encoding.Binary.Write_error _ ->
      error P2p_errors.Encoding_error

  let rec worker_loop st =
    Lwt_unix.yield () >>= fun () ->
    protect ~canceler:st.canceler begin fun () ->
      Lwt_pipe.pop st.messages >>= return
    end >>= function
    | Error [Canceled | Exn Lwt_pipe.Closed] ->
        lwt_debug "connection closed to %a"
          P2p_peer.Id.pp st.conn.info.peer_id >>= fun () ->
        Lwt.return_unit
    | Error err ->
        lwt_log_error
          "@[<v 2>error writing to %a@ %a@]"
          P2p_peer.Id.pp st.conn.info.peer_id pp_print_error err >>= fun () ->
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
                    (Error [P2p_errors.Connection_closed])) ;
            match err with
            | [ Canceled | Exn Lwt_pipe.Closed ] ->
                lwt_debug "connection closed to %a"
                  P2p_peer.Id.pp st.conn.info.peer_id >>= fun () ->
                Lwt.return_unit
            | P2p_errors.Connection_closed :: _ ->
                lwt_debug "connection closed to %a"
                  P2p_peer.Id.pp st.conn.info.peer_id >>= fun () ->
                Lwt_canceler.cancel st.canceler >>= fun () ->
                Lwt.return_unit
            | err ->
                lwt_log_error
                  "@[<v 2>error writing to %a@ %a@]"
                  P2p_peer.Id.pp st.conn.info.peer_id
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
          let size = size - Crypto.extrabytes in
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
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop st)
        ~cancel:(fun () -> Lwt_canceler.cancel st.canceler) ;
    st

  let shutdown st =
    Lwt_canceler.cancel st.canceler >>= fun () ->
    st.worker

end

type ('msg, 'meta) t = {
  conn : 'meta authenticated_connection ;
  reader : ('msg, 'meta) Reader.t ;
  writer : ('msg, 'meta) Writer.t ;
}

let equal { conn = { fd = fd2 ; _ } ; _ } { conn = { fd = fd1 ; _ } ; _ } =
  P2p_io_scheduler.id fd1 = P2p_io_scheduler.id fd2

let pp ppf { conn ; _ } = P2p_connection.Info.pp (fun _ _ -> ()) ppf conn.info
let info { conn ; _ } = conn.info
let local_metadata { conn ; _ } = conn.info.local_metadata
let remote_metadata { conn ; _ } = conn.info.remote_metadata
let private_node { conn ; _ } = conn.info.private_node

let accept
    ?incoming_message_queue_size ?outgoing_message_queue_size
    ?binary_chunks_size
    ~canceler
    conn
    encoding =
  protect begin fun () ->
    Ack.write ~canceler conn.fd conn.cryptobox_data Ack >>=? fun () ->
    Ack.read ~canceler conn.fd conn.cryptobox_data
  end ~on_error:begin fun err ->
    P2p_io_scheduler.close conn.fd >>= fun _ ->
    match err with
    | [ P2p_errors.Connection_closed ] -> fail P2p_errors.Rejected_socket_connection
    | [ P2p_errors.Decipher_error ] -> fail P2p_errors.Invalid_auth
    | err -> Lwt.return_error err
  end >>=? function
  | Ack ->
      let canceler = Lwt_canceler.create () in
      let reader =
        Reader.run ?size:incoming_message_queue_size conn encoding canceler
      and writer =
        Writer.run
          ?size:outgoing_message_queue_size ?binary_chunks_size
          conn encoding canceler
      in
      let conn = { conn ; reader ; writer } in
      Lwt_canceler.on_cancel canceler begin fun () ->
        P2p_io_scheduler.close conn.conn.fd >>= fun _ ->
        Lwt.return_unit
      end ;
      return conn
  | Nack ->
      fail P2p_errors.Rejected_socket_connection

let catch_closed_pipe f =
  Lwt.catch f begin function
    | Lwt_pipe.Closed -> fail P2p_errors.Connection_closed
    | exn -> fail (Exn exn)
  end >>= function
  | Error [Exn Lwt_pipe.Closed] ->
      fail P2p_errors.Connection_closed
  | Error _ | Ok _ as v -> Lwt.return v

let pp_json encoding ppf msg =
  Data_encoding.Json.pp ppf
    (Data_encoding.Json.construct encoding msg)

let write { writer ; conn ; _ } msg =
  catch_closed_pipe begin fun () ->
    debug "Sending message to %a: %a"
      P2p_peer.Id.pp_short conn.info.peer_id (pp_json writer.encoding) msg ;
    Lwt.return (Writer.encode_message writer msg) >>=? fun buf ->
    Lwt_pipe.push writer.messages (buf, None) >>= return
  end

let write_sync { writer ; conn ; _ } msg =
  catch_closed_pipe begin fun () ->
    let waiter, wakener = Lwt.wait () in
    debug "Sending message to %a: %a"
      P2p_peer.Id.pp_short conn.info.peer_id ( pp_json writer.encoding ) msg ;
    Lwt.return (Writer.encode_message writer msg) >>=? fun buf ->
    Lwt_pipe.push writer.messages (buf, Some wakener) >>= fun () ->
    waiter
  end

let write_now { writer ; conn ; _ } msg =
  debug "Try sending message to %a: %a"
    P2p_peer.Id.pp_short conn.info.peer_id (pp_json writer.encoding) msg ;
  Writer.encode_message writer msg >>? fun buf ->
  try Ok (Lwt_pipe.push_now writer.messages (buf, None))
  with Lwt_pipe.Closed -> Error [P2p_errors.Connection_closed]

let rec split_bytes size bytes =
  if MBytes.length bytes <= size then
    [bytes]
  else
    MBytes.sub bytes 0 size ::
    split_bytes size (MBytes.sub bytes size (MBytes.length bytes - size))

let raw_write_sync { writer ; _ } bytes =
  let bytes = split_bytes writer.binary_chunks_size bytes in
  catch_closed_pipe begin fun () ->
    let waiter, wakener = Lwt.wait () in
    Lwt_pipe.push writer.messages (bytes, Some wakener) >>= fun () ->
    waiter
  end

let is_readable { reader ; _ } =
  not (Lwt_pipe.is_empty reader.messages)
let wait_readable { reader ; _ } =
  catch_closed_pipe begin fun () ->
    Lwt_pipe.values_available reader.messages >>= return
  end
let read { reader ; _ } =
  catch_closed_pipe begin fun () ->
    Lwt_pipe.pop reader.messages
  end
let read_now { reader ; _ } =
  try Lwt_pipe.pop_now reader.messages
  with Lwt_pipe.Closed -> Some (Error [P2p_errors.Connection_closed])

let stat { conn = { fd ; _ } ; _ } = P2p_io_scheduler.stat fd

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
