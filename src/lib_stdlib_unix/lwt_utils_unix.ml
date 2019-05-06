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

let () =
  register_error_kind
    `Temporary
    ~id:"unix_error"
    ~title:"Unix error"
    ~description:"An unhandled unix exception"
    ~pp:Format.pp_print_string
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Exn (Unix.Unix_error (err, fn, _)) ->
          Some ("Unix error in " ^ fn ^ ": " ^ Unix.error_message err)
      | _ -> None)
    (fun msg -> Exn (Failure msg))

let read_bytes ?(pos = 0) ?len fd buf =
  let len = match len with None -> Bytes.length buf - pos | Some l -> l in
  let rec inner pos len =
    if len = 0 then
      Lwt.return_unit
    else
      Lwt_unix.read fd buf pos len >>= function
      | 0 -> Lwt.fail End_of_file (* other endpoint cleanly closed its connection *)
      | nb_read -> inner (pos + nb_read) (len - nb_read)
  in
  inner pos len

let read_mbytes ?(pos=0) ?len fd buf =
  let len = match len with None -> MBytes.length buf - pos | Some l -> l in
  let rec inner pos len =
    if len = 0 then
      Lwt.return_unit
    else
      Lwt_bytes.read fd buf pos len >>= function
      | 0 -> Lwt.fail End_of_file (* other endpoint cleanly closed its connection *)
      | nb_read -> inner (pos + nb_read) (len - nb_read)
  in
  inner pos len

let write_mbytes ?(pos=0) ?len descr buf =
  let len = match len with None -> MBytes.length buf - pos | Some l -> l in
  let rec inner pos len =
    if len = 0 then
      Lwt.return_unit
    else
      Lwt_bytes.write descr buf pos len >>= function
      | 0 -> Lwt.fail End_of_file (* other endpoint cleanly closed its connection *)
      | nb_written -> inner (pos + nb_written) (len - nb_written) in
  inner pos len

let write_bytes ?(pos=0) ?len descr buf =
  let len = match len with None -> Bytes.length buf - pos | Some l -> l in
  let rec inner pos len =
    if len = 0 then
      Lwt.return_unit
    else
      Lwt_unix.write descr buf pos len >>= function
      | 0 -> Lwt.fail End_of_file (* other endpoint cleanly closed its connection *)
      | nb_written -> inner (pos + nb_written) (len - nb_written) in
  inner pos len

let (>>=) = Lwt.bind

let remove_dir dir =
  let rec remove dir =
    let files = Lwt_unix.files_of_directory dir in
    Lwt_stream.iter_s
      (fun file ->
         if file = "." || file = ".." then
           Lwt.return_unit
         else begin
           let file = Filename.concat dir file in
           if Sys.is_directory file
           then remove file
           else Lwt_unix.unlink file
         end)
      files >>= fun () ->
    Lwt_unix.rmdir dir in
  if Sys.file_exists dir && Sys.is_directory dir then
    remove dir
  else
    Lwt.return_unit

let rec create_dir ?(perm = 0o755) dir =
  Lwt_unix.file_exists dir >>= function
  | false ->
      create_dir (Filename.dirname dir) >>= fun () ->
      Lwt.catch
        (fun () -> Lwt_unix.mkdir dir perm)
        (function
          | Unix.Unix_error (Unix.EEXIST, _, _) ->
              (* This is the case where the directory has been created
                 by another Lwt.t, after the call to Lwt_unix.file_exists. *)
              Lwt.return_unit
          | e -> Lwt.fail e)
  | true ->
      Lwt_unix.stat dir >>= function
      | { st_kind = S_DIR ; _ } -> Lwt.return_unit
      | _ -> Pervasives.failwith "Not a directory"

let create_file ?(perm = 0o644) name content =
  Lwt_unix.openfile name Unix.([O_TRUNC; O_CREAT; O_WRONLY]) perm >>= fun fd ->
  Lwt_unix.write_string fd content 0 (String.length content) >>= fun _ ->
  Lwt_unix.close fd

let read_file fn =
  Lwt_io.with_file fn ~mode:Input begin fun ch ->
    Lwt_io.read ch
  end



let safe_close fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)



let of_sockaddr = function
  | Unix.ADDR_UNIX _ -> None
  | Unix.ADDR_INET (addr, port) ->
      match Ipaddr_unix.of_inet_addr addr with
      | V4 addr -> Some (Ipaddr.v6_of_v4 addr, port)
      | V6 addr -> Some (addr, port)

let getaddrinfo ~passive ~node ~service =
  let open Lwt_unix in
  getaddrinfo node service
    ( AI_SOCKTYPE SOCK_STREAM ::
      (if passive then [AI_PASSIVE] else []) ) >>= fun addr ->
  let points =
    TzList.filter_map
      (fun { ai_addr ; _ } -> of_sockaddr ai_addr)
      addr in
  Lwt.return points

let getpass () =
  let open Unix in
  (* Turn echoing off and fail if we can't. *)
  let tio = tcgetattr stdin in
  let old_echo = tio.c_echo in
  let old_echonl = tio.c_echonl in
  tio.c_echo <- false ;
  tio.c_echonl <- true ;
  tcsetattr stdin TCSAFLUSH tio ;
  (* Read the passwd. *)
  let passwd = read_line () in
  (* Restore terminal. *)
  tio.c_echo <- old_echo ;
  tio.c_echonl <- old_echonl ;
  tcsetattr stdin TCSAFLUSH tio ;
  passwd

module Json = struct

  let to_root = function
    | `O ctns -> `O ctns
    | `A ctns -> `A ctns
    | `Null -> `O []
    | oth -> `A [ oth ]

  let write_file file json =
    let json = to_root json in
    protect begin fun () ->
      Lwt_io.with_file ~mode:Output file begin fun chan ->
        let str = Data_encoding.Json.to_string ~minify:false json in
        Lwt_io.write chan str >>= fun _ ->
        return_unit
      end
    end

  let read_file file =
    protect begin fun () ->
      Lwt_io.with_file ~mode:Input file begin fun chan ->
        Lwt_io.read chan >>= fun str ->
        return (Ezjsonm.from_string str :> Data_encoding.json)
      end
    end

end

module Protocol = struct

  let name = "TEZOS_PROTOCOL"

  open Protocol

  let (//) = Filename.concat

  let to_file ~dir:dirname ?hash ?env_version modules =
    let config_file =
      Data_encoding.Json.construct
        Meta.encoding
        { hash ; expected_env_version = env_version ; modules } in
    Json.write_file (dirname // name) config_file

  let of_file ~dir:dirname =
    Json.read_file (dirname // name) >>=? fun json ->
    return (Data_encoding.Json.destruct Meta.encoding json)

  let find_component dirname module_name =
    let name_lowercase = String.uncapitalize_ascii module_name in
    let implementation = dirname // name_lowercase ^ ".ml" in
    let interface = implementation ^ "i" in
    match Sys.file_exists implementation, Sys.file_exists interface with
    | false, _ -> Pervasives.failwith @@ "Not such file: " ^ implementation
    | true, false ->
        read_file implementation >|= fun implementation ->
        { name = module_name; interface = None; implementation }
    | _ ->
        read_file interface >>= fun interface ->
        read_file implementation >|= fun implementation ->
        { name = module_name; interface = Some interface; implementation }

  let read_dir dir =
    of_file ~dir >>=? fun meta ->
    Lwt_list.map_p (find_component dir) meta.modules >>= fun components ->
    let expected_env =
      match meta.expected_env_version with
      | None -> V1
      | Some v -> v in
    return (meta.hash, { expected_env ; components })

  open Lwt.Infix

  let create_files dir units =
    remove_dir dir >>= fun () ->
    create_dir dir >>= fun () ->
    Lwt_list.map_s
      (fun { name ; interface ; implementation } ->
         let name = String.lowercase_ascii name in
         let ml = dir // (name ^ ".ml") in
         let mli = dir // (name ^ ".mli") in
         create_file ml implementation >>= fun () ->
         match interface with
         | None -> Lwt.return [ml]
         | Some content ->
             create_file mli content >>= fun () ->
             Lwt.return [ mli ; ml ])
      units >>= fun files ->
    let files = List.concat files in
    Lwt.return files

  let write_dir dir ?hash (p: t) =
    create_files dir p.components >>= fun _files ->
    to_file
      ~dir
      ?hash
      ~env_version:p.expected_env
      (List.map (fun { name ; _ } -> String.capitalize_ascii name) p.components)

end

let with_tempdir name f =
  let base_dir = Filename.temp_file name "" in
  Lwt_unix.unlink base_dir >>= fun () ->
  Lwt_unix.mkdir base_dir 0o700 >>= fun () ->
  Lwt.finalize (fun () -> f base_dir) (fun () -> remove_dir base_dir)


module Socket = struct

  type addr =
    | Unix of string
    | Tcp of string * string * Unix.getaddrinfo_option list

  let handle_litteral_ipv6 host =
    (* To strip '[' and ']' when a litteral IPv6 is provided *)
    match Ipaddr.of_string host with
    | Error (`Msg _) -> host
    | Ok ipaddr -> Ipaddr.to_string ipaddr

  let connect ?(timeout=5.) = function
    | Unix path ->
        let addr = Lwt_unix.ADDR_UNIX path in
        let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
        Lwt_unix.connect sock addr >>= fun () ->
        return sock
    | Tcp (host, service, opts) ->
        let host = handle_litteral_ipv6 host in
        Lwt_unix.getaddrinfo host service opts >>= function
        | [] ->
            failwith "could not resolve host '%s'" host
        | addrs ->
            let rec try_connect acc = function
              | [] ->
                  Lwt.return
                    (Error (failure "could not connect to '%s'" host :: List.rev acc))
              | { Unix.ai_family ; ai_socktype ; ai_protocol ; ai_addr ; _ } :: addrs ->
                  let sock = Lwt_unix.socket ai_family ai_socktype ai_protocol in
                  protect ~on_error:begin fun e ->
                    Lwt_unix.close sock >>= fun () ->
                    Lwt.return_error e
                  end begin fun () ->
                    with_timeout (Lwt_unix.sleep timeout) (fun _c ->
                        Lwt_unix.connect sock ai_addr >>= fun () ->
                        return sock)
                  end >>= function
                  | Ok sock -> return sock
                  | Error e ->
                      try_connect (e @ acc) addrs in
            try_connect [] addrs

  let bind ?(backlog = 10) = function
    | Unix path ->
        let addr = Lwt_unix.ADDR_UNIX path in
        let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
        Lwt_unix.bind sock addr >>= fun () ->
        Lwt_unix.listen sock backlog ;
        return [sock]
    | Tcp (host, service, opts) ->
        Lwt_unix.getaddrinfo
          (handle_litteral_ipv6 host) service (AI_PASSIVE :: opts) >>= function
        | [] -> failwith "could not resolve host '%s'" host
        | addrs ->
            let do_bind { Unix.ai_family ; ai_socktype ; ai_protocol ; ai_addr ; _ } =
              let sock = Lwt_unix.socket ai_family ai_socktype ai_protocol in
              Lwt_unix.setsockopt sock SO_REUSEADDR true ;
              Lwt_unix.bind sock ai_addr >>= fun () ->
              Lwt_unix.listen sock backlog ;
              return sock in
            map_s do_bind addrs

  type error +=
    | Encoding_error
    | Decoding_error

  let () =
    register_error_kind `Permanent
      ~id: "signer.encoding_error"
      ~title: "Encoding_error"
      ~description: "Error while encoding a remote signer message"
      ~pp: (fun ppf () ->
          Format.fprintf ppf "Could not encode a remote signer message")
      Data_encoding.empty
      (function Encoding_error -> Some () | _ -> None)
      (fun () -> Encoding_error) ;
    register_error_kind `Permanent
      ~id: "signer.decoding_error"
      ~title: "Decoding_error"
      ~description: "Error while decoding a remote signer message"
      ~pp: (fun ppf () ->
          Format.fprintf ppf "Could not decode a remote signer message")
      Data_encoding.empty
      (function Decoding_error -> Some () | _ -> None)
      (fun () -> Decoding_error)

  let message_len_size = 2

  let send fd encoding message =
    let encoded_message_len = Data_encoding.Binary.length encoding message in
    fail_unless
      (encoded_message_len < 1 lsl (message_len_size * 8))
      Encoding_error >>=? fun () ->
    (* len is the length of int16 plus the length of the message we want to send *)
    let len = message_len_size + encoded_message_len in
    let buf = MBytes.create len in
    match Data_encoding.Binary.write
            encoding message buf message_len_size encoded_message_len with
    | None ->
        fail Encoding_error
    | Some last ->
        fail_unless (last = len) Encoding_error >>=? fun () ->
        (* we set the beginning of the buf with the length of what is next *)
        MBytes.set_int16 buf 0 encoded_message_len ;
        write_mbytes fd buf >>= fun () ->
        return_unit

  let recv fd encoding =
    let header_buf = MBytes.create message_len_size in
    read_mbytes ~len:message_len_size fd header_buf >>= fun () ->
    let len = MBytes.get_uint16 header_buf 0 in
    let buf = MBytes.create len in
    read_mbytes ~len fd buf >>= fun () ->
    match Data_encoding.Binary.read encoding buf 0 len with
    | None ->
        fail Decoding_error
    | Some (read_len, message) ->
        if read_len <> len then
          fail Decoding_error
        else
          return message

end


let rec retry ?(log=(fun _ -> Lwt.return_unit)) ?(n=5) ?(sleep=1.) f =
  f () >>= function
  | Ok r -> Lwt.return_ok r
  | (Error error) as x ->
      if n > 0 then
        begin
          log error >>= fun () ->
          Lwt_unix.sleep sleep >>= fun () ->
          retry ~log ~n:(n-1) ~sleep f
        end
      else
        Lwt.return x

