(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_shell
open Helpers_logger

let previous_gs = ref None

let cleanup () =
  ignore @@ Sys.command @@ "rm -rf " ^ Helpers_constants.test_folder

let get_global_state () =
  match !previous_gs with
  | Some gs ->
      State.Net.all gs >>= fun ls ->
      Lwt_list.iter_p (State.Net.destroy gs) ls >>= fun () ->
      return gs
  | None ->
      Lwt.catch (
        fun () ->
          State.read
            ~store_root: Helpers_constants.store_root
            ~context_root: Helpers_constants.context_root
            () >>= function
          | Ok init_state -> (
              previous_gs := Some init_state ;
              Lwt.return @@ Ok init_state
            )| Error errors -> (
              lwt_warn "Errors !" >>= fun () ->
              lwt_warn "Error when building global state ...%a" pp_print_error errors >>= fun () ->
              Lwt.return @@ Error errors
            )
      ) (function
          | e -> Logger.lwt_warn "Error !" >>= fun () ->
              Lwt.fail e
        )

let get_activation_block baker context_hash head =
  let open Tezos_embedded_raw_protocol_genesis in
  State.Block.context head >>= fun context ->
  Data.Pubkey.set_pubkey context baker.Helpers_account.pub >>= fun context ->
  let shell_header =
    Helpers_misc.get_block_header
      head
      Helpers_misc.no_ops_hash
      (State.Block.fitness head)
      context_hash
      (Time.now ())
  in
  let fitness =
    Tezos_embedded_raw_protocol_alpha.Fitness_repr.from_int64 100L in
  let command: Data.Command.t =
    Data.Command.Activate({protocol = Helpers_constants.alpha_hash ; fitness}) in
  let content_bytes = Data.Command.forge shell_header command in
  let signature = Ed25519.sign baker.ppk content_bytes in
  let proto = (command , signature) in
  let proto_bytes =
    Data_encoding.Binary.to_bytes
      Data.Command.signed_encoding
      proto in
  let raw_block: Block_header.t = {
    shell = shell_header ;
    proto = proto_bytes
  } in
  return (context , raw_block)

let get_alpha () =
  get_global_state () >>=? fun global_state ->
  State.Net.create global_state Helpers_constants.genesis >>= fun state ->
  Tezos_shell.Chain.head state >>= fun head ->
  let baker = Helpers_account.new_account () in
  let rec attempt context_hash =
    begin
      get_activation_block baker context_hash head >>=? fun (context , raw_block) ->
      Tezos_storage.Context.get_protocol context >>= fun protocol_hash ->
      let (module Protocol) = Helpers_misc.get_protocol protocol_hash in
      Protocol.begin_application
        ~predecessor_context: context
        ~predecessor_timestamp: (State.Block.timestamp head)
        ~predecessor_fitness: (State.Block.fitness head)
        raw_block
      >>=? fun app ->
      Protocol.finalize_block app >>=? fun result ->
      State.Block.store state raw_block [[]] result >>=? fun opt_block ->
      return (opt_block , result)
    end >>= function
    | Ok v -> return v
    | Error [ State.Block.Inconsistent_hash (got , _) ] ->
        attempt got
    | Error err ->
        Error_monad.pp_print_error Format.err_formatter err ;
        Lwt.return (Error err) in
  attempt Context_hash.zero >>=? fun (opt_block , result) ->
  Error_monad.protect (fun () -> return (Option.unopt_exn (Failure "get_alpha") opt_block)) >>=? fun block ->
  Tezos_shell.Chain.set_head state block >>= fun _ ->
  return (global_state , state , result)

let get_sandbox () =
  Data_encoding_ezjsonm.read_file
    "src/proto_alpha/lib_protocol/test/sandbox.json" >>= function
  | Ok x -> Lwt.return x
  | Error _ ->
      Data_encoding_ezjsonm.read_file "test/sandbox.json" >>= fun x ->
      Lwt.return @@ Helpers_assert.no_error ~msg:__LOC__ x

open Helpers_assert

let main () =
  cleanup () ;
  ignore @@ Unix.mkdir Helpers_constants.test_folder 0o777 ;
  ignore @@ Unix.mkdir Helpers_constants.store_root 0o777 ;
  get_alpha () >>=? fun (_gs, s, r) ->
  let context = r.context in
  Tezos_shell.Chain.head s >>= fun head ->
  let hash = State.Block.hash head in
  let block_shell_header = State.Block.shell_header head in
  get_sandbox () >>= fun json ->
  Main.configure_sandbox context @@ Some json >>=?? fun context ->
  Helpers_block.empty block_shell_header hash Int32.zero 0 context >>= Helpers_assert.wrap

let () = at_exit cleanup
