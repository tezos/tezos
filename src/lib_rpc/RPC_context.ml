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

open Error_monad

class type ['pr] gen_simple = object
  method call_service :
    'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
    'p -> 'q -> 'i -> 'o tzresult Lwt.t
end

class type ['pr] gen_streamed = object
  method call_streamed_service :
    'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t
end

class type ['pr] gen = object
  inherit ['pr] gen_simple
  inherit ['pr] gen_streamed
end

class type simple = object
  inherit [unit] gen_simple
end

class type streamed = object
  inherit [unit] gen_streamed
end

class type t = object
  inherit simple
  inherit streamed
end

type ('o, 'e) rest_result =
  [ `Ok of 'o
  | `Conflict of 'e
  | `Error of 'e
  | `Forbidden of 'e
  | `Not_found of 'e
  | `Unauthorized of 'e ] tzresult

class type json = object
  inherit t
  method generic_json_call :
    RPC_service.meth ->
    ?body:Data_encoding.json ->
    Uri.t ->
    (Data_encoding.json, Data_encoding.json option)
      rest_result Lwt.t
  method base : Uri.t
end


type error +=
  | Not_found of { meth: RPC_service.meth ;
                   uri: Uri.t }
  | Generic_error of { meth: RPC_service.meth ;
                       uri: Uri.t }

let base = Uri.make ~scheme:"ocaml" ()
let not_found s p q =
  let { RPC_service.meth ; uri ; _ } =
    RPC_service.forge_partial_request s ~base p q in
  fail (Not_found { meth ; uri })

let generic_error s p q =
  let { RPC_service.meth ; uri ; _ } =
    RPC_service.forge_partial_request s ~base p q in
  fail (Generic_error { meth ; uri })

class ['pr] of_directory (dir : 'pr RPC_directory.t) = object
  method call_service : 'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
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
    ([< Resto.meth ] as 'm, 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
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

let () =
  let open Data_encoding in
  let uri_encoding =
    conv
      Uri.to_string
      Uri.of_string
      string in
  register_error_kind
    `Branch
    ~id:"RPC_context.Not_found"
    ~title:"RPC lookup failed"
    ~description:"RPC lookup failed. No RPC exists at the URL or the RPC tried to access non-existent data."
    (obj2
       (req "method" RPC_service.meth_encoding)
       (req "uri" uri_encoding))
    ~pp:(fun ppf (meth, uri) ->
        Format.fprintf ppf "Did not find service: %s %a" (RPC_service.string_of_meth meth) Uri.pp_hum uri)
    (function Not_found { meth ; uri } -> Some (meth, uri)
            | _ -> None)
    (fun (meth, uri) -> Not_found { meth ; uri })
