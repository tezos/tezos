
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

