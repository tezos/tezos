(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module LC = Lwt_condition

open Lwt.Infix
open Logging.Core

let may ~f = function
  | None -> Lwt.return_unit
  | Some x -> f x

let never_ending = fst (Lwt.wait ())

type trigger =
  | Absent
  | Present
  | Waiting of unit Lwt.t * unit Lwt.u

let trigger () : (unit -> unit) * (unit -> unit Lwt.t) =
  let state = ref Absent in
  let trigger () =
    match !state with
    | Absent -> state := Present
    | Present -> ()
    | Waiting (_waiter, wakener) ->
        state := Absent;
        Lwt.wakeup wakener ()
  in
  let wait () =
    match !state with
    | Absent ->
        let waiter, wakener = Lwt.wait () in
        state := Waiting (waiter, wakener) ;
        waiter
    | Present ->
        state := Absent;
        Lwt.return_unit
    | Waiting (waiter, _wakener)  ->
        waiter
  in
  trigger, wait

(* A worker launcher, takes a cancel callback to call upon *)
let worker name ~run ~cancel =
  let stop = LC.create () in
  let fail e =
    log_error "%s worker failed with %s" name (Printexc.to_string e) ;
    cancel ()
  in
  let waiter = LC.wait stop in
  log_info "%s worker started" name ;
  Lwt.async
    (fun () ->
       Lwt.catch run fail >>= fun () ->
       LC.signal stop ();
       Lwt.return ()) ;
  waiter >>= fun () ->
  log_info "%s worker ended" name ;
  Lwt.return ()


let rec chop k l =
  if k = 0 then l else begin
    match l with
    | _::t -> chop (k-1) t
    | _ -> assert false
  end
let stable_sort cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> Lwt.return (List.rev_append l2 accu)
    | l1, [] -> Lwt.return (List.rev_append l1 accu)
    | h1::t1, h2::t2 ->
        cmp h1 h2 >>= function
        | x when x <= 0 -> rev_merge t1 l2 (h1::accu)
        | _             -> rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> Lwt.return (List.rev_append l2 accu)
    | l1, [] -> Lwt.return (List.rev_append l1 accu)
    | h1::t1, h2::t2 ->
        cmp h1 h2 >>= function
        | x when x > 0 -> rev_merge_rev t1 l2 (h1::accu)
        | _            -> rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ -> begin
        cmp x1 x2 >|= function
        | x when x <= 0 -> [x1; x2]
        | _             -> [x2; x1]
      end
    | 3, x1 :: x2 :: x3 :: _ -> begin
        cmp x1 x2 >>= function
        | x when x <= 0 -> begin
            cmp x2 x3 >>= function
            | x when x <= 0 -> Lwt.return [x1; x2; x3]
            | _ -> cmp x1 x3 >|= function
              | x when x <= 0 -> [x1; x3; x2]
              | _ -> [x3; x1; x2]
          end
        | _ -> begin
            cmp x1 x3 >>= function
            | x when x <= 0 -> Lwt.return [x2; x1; x3]
            | _ -> cmp x2 x3 >|= function
              | x when x <= 0 -> [x2; x3; x1]
              | _ -> [x3; x2; x1]
          end
      end
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let l2 = chop n1 l in
        rev_sort n1 l >>= fun s1 ->
        rev_sort n2 l2 >>= fun s2 ->
        rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ -> begin
        cmp x1 x2 >|= function
        | x when x > 0 -> [x1; x2]
        | _ -> [x2; x1]
      end
    | 3, x1 :: x2 :: x3 :: _ -> begin
        cmp x1 x2 >>= function
        | x when x > 0 -> begin
            cmp x2 x3 >>= function
            | x when x > 0 -> Lwt.return [x1; x2; x3]
            | _ ->
                cmp x1 x3 >|= function
                | x when x > 0 -> [x1; x3; x2]
                | _ -> [x3; x1; x2]
          end
        | _ -> begin
            cmp x1 x3 >>= function
            | x when x > 0 -> Lwt.return [x2; x1; x3]
            | _ ->
                cmp x2 x3 >|= function
                | x when x > 0 -> [x2; x3; x1]
                | _ -> [x3; x2; x1]
          end
      end
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let l2 = chop n1 l in
        sort n1 l >>= fun s1 ->
        sort n2 l2 >>= fun s2 ->
        rev_merge s1 s2 []
  in
  let len = List.length l in
  if len < 2 then Lwt.return l else sort len l

let sort = stable_sort

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
           Lwt.return ()
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
    Lwt.return ()

let rec create_dir ?(perm = 0o755) dir =
  Lwt_unix.file_exists dir >>= function
  | false ->
      create_dir (Filename.dirname dir) >>= fun () ->
      Lwt_unix.mkdir dir perm
  | true ->
      Lwt_unix.stat dir >>= function
      | { st_kind = S_DIR ; _ } -> Lwt.return_unit
      | _ -> failwith "Not a directory"

let create_file ?(perm = 0o644) name content =
  Lwt_unix.openfile name Unix.([O_TRUNC; O_CREAT; O_WRONLY]) perm >>= fun fd ->
  Lwt_unix.write_string fd content 0 (String.length content) >>= fun _ ->
  Lwt_unix.close fd

let safe_close fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)

open Error_monad

type error += Canceled

let protect ?on_error ?canceler t =
  let cancelation =
    match canceler with
    | None -> never_ending
    | Some canceler ->
        (Lwt_canceler.cancelation canceler >>= fun () ->
         fail Canceled ) in
  let res =
    Lwt.pick [ cancelation ;
               Lwt.catch t (fun exn -> fail (Exn exn)) ] in
  res >>= function
  | Ok _ -> res
  | Error err ->
      let canceled =
        Option.unopt_map canceler ~default:false ~f:Lwt_canceler.canceled in
      let err = if canceled then [Canceled] else err in
      match on_error with
      | None -> Lwt.return (Error err)
      | Some on_error ->
          Lwt.catch (fun () -> on_error err) (fun exn -> fail (Exn exn))

type error += Timeout

let () =
  Error_monad.register_error_kind
    `Temporary
    ~id:"utils.Timeout"
    ~title:"Timeout"
    ~description:"Timeout"
    Data_encoding.unit
    (function Timeout -> Some () | _ -> None)
    (fun () -> Timeout)

let with_timeout ?(canceler = Lwt_canceler.create ()) timeout f =
  let timeout = Lwt_unix.sleep timeout in
  let target = f canceler in
  Lwt.choose [ timeout ; (target >|= fun _ -> ()) ] >>= fun () ->
  Lwt_unix.yield () >>= fun () ->
  if Lwt.state target <> Lwt.Sleep then begin
    Lwt.cancel timeout ;
    target
  end else begin
    Lwt_canceler.cancel canceler >>= fun () ->
    fail Timeout
  end

let unless cond f =
  if cond then Lwt.return () else f ()

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
