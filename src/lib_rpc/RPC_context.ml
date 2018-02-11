(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

class type simple = object
  method call_service :
    'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    'p -> 'q -> 'i -> 'o tzresult Lwt.t
end

class type streamed = object
  method call_streamed_service :
    'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t
end

class type t = object
  inherit simple
  inherit streamed
end

type error +=
  | Not_found of { meth: RPC_service.meth ;
                   uri: Uri.t }
  | Generic_error of { meth: RPC_service.meth ;
                       uri: Uri.t }

let base = Uri.make ~scheme:"ocaml" ()
let not_found s p q =
  let { RPC_service.meth ; uri ; _ } =
    RPC_service.forge_request s ~base p q in
  fail (Not_found { meth ; uri })

let generic_error s p q =
  let { RPC_service.meth ; uri ; _ } =
    RPC_service.forge_request s ~base p q in
  fail (Generic_error { meth ; uri })

let of_directory (dir : unit RPC_directory.t) : t = object
  method call_service : 'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    'p -> 'q -> 'i -> 'o tzresult Lwt.t =
    fun s p q i ->
      RPC_directory.transparent_lookup dir s p q i >>= function
      | `Ok v -> return v
      | `OkStream { next ; shutdown } -> begin
          next () >>= function
          | Some v -> shutdown () ; return v
          | None -> shutdown () ; not_found s p q
        end
      | `Not_found None -> not_found s p q
      | `Unauthorized (Some err)
      | `Forbidden (Some err)
      | `Not_found (Some err)
      | `Conflict (Some err)
      | `Error (Some err) -> Lwt.return_error err
      | `Unauthorized None
      | `Error None
      | `Forbidden None
      | `Created _
      | `Conflict None
      | `No_content -> generic_error s p q
  method call_streamed_service : 'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t =
    fun s ~on_chunk ~on_close p q i ->
      RPC_directory.transparent_lookup dir s p q i >>= function
      | `OkStream { next; shutdown } ->
          let rec loop () =
            next () >>= function
            | None -> on_close () ; Lwt.return_unit
            | Some v -> on_chunk v ; loop () in
          let _ = loop () in
          return shutdown
      | `Ok v ->
          on_chunk v ; on_close () ;
          return (fun () -> ())
      | `Not_found None -> not_found s p q
      | `Unauthorized (Some err)
      | `Forbidden (Some err)
      | `Not_found (Some err)
      | `Conflict (Some err)
      | `Error (Some err) -> Lwt.return_error err
      | `Unauthorized None
      | `Error None
      | `Forbidden None
      | `Created _
      | `Conflict None
      | `No_content -> generic_error s p q
end

let make_call s (ctxt : #simple) = ctxt#call_service s
let make_call1 s ctxt x = make_call s ctxt ((), x)
let make_call2 s ctxt x y = make_call s ctxt (((), x), y)
let make_call3 s ctxt x y z = make_call s ctxt ((((), x), y), z)

type stopper = unit -> unit

let make_streamed_call s (ctxt : #streamed) p q i =
  let stream, push = Lwt_stream.create () in
  let on_chunk v = push (Some v)
  and on_close () = push None in
  ctxt#call_streamed_service s ~on_chunk ~on_close p q i >>=? fun close ->
  return (stream, close)
