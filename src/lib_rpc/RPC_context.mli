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

class ['pr] of_directory : 'pr RPC_directory.t -> ['pr] gen

type error +=
  | Not_found of { meth: RPC_service.meth ;
                   uri: Uri.t }
  | Generic_error of { meth: RPC_service.meth ;
                       uri: Uri.t }

val make_call :
  ([< Resto.meth ], unit, 'p, 'q, 'i, 'o) RPC_service.t ->
  #simple -> 'p -> 'q -> 'i -> 'o tzresult Lwt.t

val make_call1 :
  ([< Resto.meth ], unit, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  #simple -> 'a -> 'q -> 'i -> 'o tzresult Lwt.t

val make_call2 :
  ([< Resto.meth ], unit, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  #simple -> 'a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t

val make_call3 :
  ([< Resto.meth ], unit, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  #simple -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t

type stopper = unit -> unit

val make_streamed_call :
  ([< Resto.meth ], unit, 'p, 'q, 'i, 'o) RPC_service.t ->
  #streamed -> 'p -> 'q ->  'i ->
  ('o Lwt_stream.t * stopper) tzresult Lwt.t

