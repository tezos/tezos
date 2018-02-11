(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

(** Representation of blocks independent from the State module *)

type shell_header = Block_header.shell_header
type tezos_header = Block_header.t
type protocol_header = Alpha_context.Block_header.proto_header
type operation_header = Operation.shell_header

(** Block before application *)
type init_block = {
  pred_block_hash : Block_hash.t;
  pred_shell_header : shell_header;
  proto_header : protocol_header;
  op_header : operation_header;
  sourced_operations :
    (Main.operation * Helpers_account.t) list;
  operation_hashs : Operation_hash.t list;
  proto_header_bytes : MBytes.t;
  timestamp : Time.t;
  level : Int32.t;
  context : Context.t;
}

(** Result of the application of a block *)
type result = {
  tezos_header : tezos_header;
  hash : Block_hash.t;
  level : Int32.t;
  validation : Updater.validation_result;
  tezos_context : Alpha_context.t;
}
val get_op_header_res : result -> operation_header
val get_proto_header : int -> protocol_header
val get_op_header : Block_hash.t -> operation_header
val make_sourced_operation :
  Operation.shell_header ->
  Alpha_context.proto_operation *
  Helpers_account.t ->
  ((Proto_alpha.Main.operation * Helpers_account.t) * Operation_hash.t) proto_tzresult
val init :
  shell_header -> Block_hash.t -> Int32.t -> int ->
  (Alpha_context.proto_operation * Helpers_account.t) list ->
  Context.t -> init_block proto_tzresult
val init_of_result :
  ?priority:int -> res:result ->
  ops:(Alpha_context.proto_operation * Helpers_account.t) list ->
  init_block proto_tzresult
val get_level : string option -> int32
val get_header_hash :
  init_block -> Updater.validation_result ->
  result proto_tzresult Lwt.t
val begin_construction_pre :
  init_block -> Main.validation_state proto_tzresult Lwt.t
val make : init_block -> result proto_tzresult Lwt.t
val make_init :
  shell_header -> Block_hash.t -> Int32.t -> int ->
  (Alpha_context.proto_operation * Helpers_account.t) list ->
  Context.t -> result proto_tzresult Lwt.t
val of_res :
  ?priority:int ->
  ?ops:(Alpha_context.proto_operation * Helpers_account.t) list ->
  res:result ->
  unit -> result proto_tzresult Lwt.t
val endorsement :
  shell_header -> Block_hash.t -> Int32.t -> int ->
  Helpers_account.t -> Context.t -> int ->
  result proto_tzresult Lwt.t
val endorsement_of_res :
  result -> Helpers_account.t -> int -> ?priority:int -> res:result ->
  unit -> result proto_tzresult Lwt.t
val empty :
  shell_header -> Block_hash.t -> Int32.t -> int ->
  Context.t -> result proto_tzresult Lwt.t
