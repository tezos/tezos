(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

(* A non exception-based cancelation mechanism. Builds a [cancelation]
   thread to bind / pick on, awoken when a cancelation is requested by
   [cancel ()]. [on_cancel cb] registers a callback to be called at
   cancelation. [cancel ()] finishes when all calbacks have completed
   (sequentially), instantly when called more than once. *)
let canceler ()
  : (unit -> unit Lwt.t) *
    (unit -> unit Lwt.t) *
    ((unit -> unit Lwt.t) -> unit) =
  let cancelation = LC.create () in
  let cancelation_complete = LC.create () in
  let cancel_hook = ref (fun () -> Lwt.return ()) in
  let canceling = ref false and canceled = ref false  in
  let cancel () =
    if !canceled then
      Lwt.return ()
    else if !canceling then
      LC.wait cancelation_complete
    else begin
      canceling := true ;
      LC.broadcast cancelation () ;
      Lwt.finalize
        !cancel_hook
        (fun () ->
           canceled := true ;
           LC.broadcast cancelation_complete () ;
           Lwt.return ()) >>= fun () ->
      Lwt.return_unit
    end
  in
  let on_cancel cb =
    let hook = !cancel_hook in
    cancel_hook := (fun () -> hook () >>= cb) ;
  in
  let cancelation () =
    if !canceling then Lwt.return ()
    else LC.wait cancelation
  in
  cancelation, cancel, on_cancel

module Canceler = struct

  type t = {
    cancelation: unit Lwt_condition.t ;
    cancelation_complete: unit Lwt_condition.t ;
    mutable cancel_hook: unit -> unit Lwt.t ;
    mutable canceling: bool ;
    mutable canceled: bool ;
  }

  let create () =
    let cancelation = LC.create () in
    let cancelation_complete = LC.create () in
    { cancelation ; cancelation_complete ;
      cancel_hook = (fun () -> Lwt.return ()) ;
      canceling = false ;
      canceled = false ;
    }

  let cancel st =
    if st.canceled then
      Lwt.return ()
    else if st.canceling then
      LC.wait st.cancelation_complete
    else begin
      st.canceling <- true ;
      LC.broadcast st.cancelation () ;
      Lwt.finalize
        st.cancel_hook
        (fun () ->
           st.canceled <- true ;
           LC.broadcast st.cancelation_complete () ;
           Lwt.return ())
    end

  let on_cancel st cb =
    let hook = st.cancel_hook in
    st.cancel_hook <- (fun () -> hook () >>= cb)

  let cancelation st =
    if st.canceling then Lwt.return ()
    else LC.wait st.cancelation

  let canceled st = st.canceling

end

module Idle_waiter = struct

  type t =
    { mutable pending_tasks : unit Lwt.u list ;
      mutable pending_idle : (unit -> unit Lwt.t) list ;
      mutable running_tasks : int ;
      mutable running_idle : bool ;
      mutable prevent_tasks : bool }

  let create () =
    { pending_tasks = [] ;
      pending_idle = [] ;
      running_tasks = 0 ;
      running_idle = false ;
      prevent_tasks = false }

  let rec may_run_idle_tasks w =
    if w.running_tasks = 0 && not w.running_idle then
      match w.pending_idle with
      | [] -> ()
      | pending_idle ->
          w.running_idle <- true ;
          w.prevent_tasks <- false ;
          w.pending_idle <- [] ;
          Lwt.async (fun () ->
              let pending_idle = List.rev pending_idle in
              Lwt_list.iter_s (fun f -> f ()) pending_idle >>= fun () ->
              w.running_idle <- false ;
              let pending_tasks = List.rev w.pending_tasks in
              w.pending_tasks <- [] ;
              List.iter (fun u -> Lwt.wakeup u ()) pending_tasks ;
              may_run_idle_tasks w ;
              Lwt.return ())

  let wrap_error f =
    Lwt.catch
      (fun () -> f () >>= fun r -> Lwt.return (Ok r))
      (fun exn -> Lwt.return (Error exn))

  let unwrap_error = function
    | Ok r -> Lwt.return r
    | Error exn -> Lwt.fail exn

  let wakeup_error u = function
    | Ok r -> Lwt.wakeup u r
    | Error exn -> Lwt.wakeup_exn u exn

  let rec task w f =
    if w.running_idle || w.prevent_tasks then
      let t, u = Lwt.task () in
      w.pending_tasks <- u :: w.pending_tasks ;
      t >>= fun () -> task w f
    else begin
      w.running_tasks <- w.running_tasks + 1 ;
      wrap_error f >>= fun res ->
      w.running_tasks <- w.running_tasks - 1 ;
      may_run_idle_tasks w ;
      unwrap_error res
    end

  let when_idle w f =
    let t, u = Lwt.task () in
    let canceled = ref false in
    Lwt.on_cancel t (fun () -> canceled := true) ;
    let f () =
      if !canceled then
        Lwt.return ()
      else
        wrap_error f >>= fun res ->
        wakeup_error u res ;
        Lwt.return () in
    w.pending_idle <- f :: w.pending_idle ;
    may_run_idle_tasks w ;
    t

  let force_idle w f =
    w.prevent_tasks <- true ;
    when_idle w f

end

type trigger =
  | Absent
  | Present
  | Waiting of unit Lwt.u

let trigger () : (unit -> unit) * (unit -> unit Lwt.t) =
  let state = ref Absent in
  let trigger () =
    match !state with
    | Absent -> state := Present
    | Present -> ()
    | Waiting u ->
        state := Absent;
        Lwt.wakeup u ()
  in
  let wait () =
    match !state with
    | Absent ->
        let waiter, u = Lwt.wait () in
        state := Waiting u;
        waiter
    | Present ->
        state := Absent;
        Lwt.return_unit
    | Waiting u ->
        Lwt.waiter_of_wakener u
  in
  trigger, wait

type 'a queue =
  | Absent
  | Present of 'a list ref
  | Waiting of 'a list Lwt.u

let queue () : ('a -> unit) * (unit -> 'a list Lwt.t) =
  let state = ref Absent in
  let queue v =
    match !state with
    | Absent -> state := Present (ref [v])
    | Present r -> r := v :: !r
    | Waiting u ->
        state := Absent;
        Lwt.wakeup u [v]
  in
  let wait () =
    match !state with
    | Absent ->
        let waiter, u = Lwt.wait () in
        state := Waiting u;
        waiter
    | Present r ->
        state := Absent;
        Lwt.return (List.rev !r)
    | Waiting u ->
        Lwt.waiter_of_wakener u
  in
  queue, wait

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
      | {st_kind = S_DIR} -> Lwt.return_unit
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
        ( Canceler.cancelation canceler >>= fun () ->
          fail Canceled ) in
  let res =
    Lwt.pick [ cancelation ;
               Lwt.catch t (fun exn -> fail (Exn exn)) ] in
  res >>= function
  | Ok _ -> res
  | Error err ->
      let canceled =
        Utils.unopt_map canceler ~default:false ~f:Canceler.canceled in
      let err = if canceled then [Canceled] else err in
      match on_error with
      | None -> Lwt.return (Error err)
      | Some on_error -> on_error err

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

let with_timeout ?(canceler = Canceler.create ()) timeout f =
  let timeout = Lwt_unix.sleep timeout in
  let target = f canceler in
  Lwt.choose [ timeout ; (target >|= fun _ -> ()) ] >>= fun () ->
  Lwt_unix.yield () >>= fun () ->
  if Lwt.state target <> Lwt.Sleep then begin
    Lwt.cancel timeout ;
    target
  end else begin
    Canceler.cancel canceler >>= fun () ->
    fail Timeout
  end

let unless cond f =
  if cond then Lwt.return () else f ()

module Lock_file = struct
  let create_inner
      lock_command
      ?(close_on_exec=true)
      ?(unlink_on_exit=false) fn =
    protect begin fun () ->
      Lwt_unix.openfile fn Unix.[O_CREAT ; O_WRONLY; O_TRUNC] 0o644 >>= fun fd ->
      if close_on_exec then Lwt_unix.set_close_on_exec fd ;
      Lwt_unix.lockf fd lock_command 0 >>= fun () ->
      if unlink_on_exit then
        Lwt_main.at_exit (fun () -> Lwt_unix.unlink fn) ;
      let pid_str = string_of_int @@ Unix.getpid () in
      Lwt_unix.write_string fd pid_str 0 (String.length pid_str) >>= fun _ ->
      return ()
    end

  let create = create_inner Unix.F_TLOCK

  let blocking_create
      ?timeout
      ?(close_on_exec=true)
      ?(unlink_on_exit=false) fn =
    let create () =
      create_inner Unix.F_LOCK ~close_on_exec ~unlink_on_exit fn in
    match timeout with
    | None -> create ()
    | Some duration -> with_timeout duration (fun _ -> create ())

  let is_locked fn =
    if not @@ Sys.file_exists fn then return false else
      protect begin fun () ->
        Lwt_unix.openfile fn [Unix.O_RDONLY] 0o644 >>= fun fd ->
        Lwt.finalize (fun () ->
            Lwt.try_bind
              (fun () -> Lwt_unix.(lockf fd F_TEST 0))
              (fun () -> return false)
              (fun _ -> return true))
          (fun () -> Lwt_unix.close fd)
      end

  let get_pid fn =
    let open Lwt_io in
    protect begin fun () ->
      with_file ~mode:Input fn begin fun ic ->
        read ic >>= fun content ->
        return (int_of_string content)
      end
    end
end

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
    Utils.filter_map
      (fun { ai_addr } -> of_sockaddr ai_addr)
      addr in
  Lwt.return points
