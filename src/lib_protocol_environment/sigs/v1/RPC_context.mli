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

type t = Updater.rpc_context

class type ['pr] simple = object
  method call_proto_service0 :
    'm 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, t, t, 'q, 'i, 'o) RPC_service.t ->
    'pr -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
  method call_proto_service1 :
    'm 'a 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, t, t * 'a, 'q, 'i, 'o) RPC_service.t ->
    'pr -> 'a -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
  method call_proto_service2 :
    'm 'a 'b 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, t, (t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
    'pr -> 'a -> 'b -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
  method call_proto_service3 :
    'm 'a 'b 'c 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, t, ((t * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
    'pr -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
end

val make_call0:
  ([< RPC_service.meth ], t, t, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'q -> 'i -> 'o shell_tzresult Lwt.t

val make_call1:
  ([< RPC_service.meth ], t, t * 'a, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'a -> 'q -> 'i -> 'o shell_tzresult Lwt.t

val make_call2:
  ([< RPC_service.meth ], t, (t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'a -> 'b -> 'q -> 'i -> 'o shell_tzresult Lwt.t

val make_call3:
  ([< RPC_service.meth ], t, ((t * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o shell_tzresult Lwt.t


val make_opt_call0:
  ([< RPC_service.meth ], t, t, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'q -> 'i -> 'o option shell_tzresult Lwt.t

val make_opt_call1:
  ([< RPC_service.meth ], t, t * 'a, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'a -> 'q -> 'i -> 'o option shell_tzresult Lwt.t

val make_opt_call2:
  ([< RPC_service.meth ], t, (t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'a -> 'b -> 'q -> 'i -> 'o option shell_tzresult Lwt.t

val make_opt_call3:
  ([< RPC_service.meth ], t, ((t * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple -> 'pr -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o option shell_tzresult Lwt.t
