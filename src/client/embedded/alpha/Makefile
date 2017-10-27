
PROTO_VERSION := alpha

CLIENT_INTFS := \
  michelson_macros.mli \
  michelson_parser.mli \
  client_proto_rpcs.mli \
  client_proto_args.mli \
  client_proto_contracts.mli \
  client_proto_programs.mli \
  client_proto_context.mli \
  client_proto_nonces.mli \
  client_proto_main.mli \
  client_mining_blocks.mli \
  client_mining_operations.mli \
  client_mining_endorsement.mli \
  client_mining_denunciation.mli \
  client_mining_revelation.mli \
  client_mining_forge.mli \
  client_mining_daemon.mli \
  client_mining_main.mli \

CLIENT_IMPLS := \
  client_proto_alpha.ml \
  script_located_ir.ml \
  michelson_macros.ml \
  michelson_parser.ml \
  client_proto_rpcs.ml \
  client_proto_args.ml \
  client_proto_contracts.ml \
  client_proto_programs.ml \
  client_proto_context.ml \
  client_proto_nonces.ml \
  client_proto_main.ml \
  client_mining_blocks.ml \
  client_mining_operations.ml \
  client_mining_endorsement.ml \
  client_mining_denunciation.ml \
  client_mining_revelation.ml \
  client_mining_forge.ml \
  client_mining_daemon.ml \
  client_mining_main.ml

include ../Makefile.shared

${OBJS_DEPS} ${OBJS}: OPENED_MODULES += Client_proto_alpha Tezos_context
client_proto_alpha.ml.deps client_proto_alpha.cmx: OPENED_MODULES :=
