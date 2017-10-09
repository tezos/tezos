
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
  client_proto_main.mli

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
  client_proto_main.ml

-include baker/Makefile

include ../Makefile.shared

${OBJS_DEPS} ${OBJS}: OPENED_MODULES += Client_proto_alpha Tezos_context
client_proto_alpha.ml.deps client_proto_alpha.cmx: OPENED_MODULES :=

.PHONY: clean
clean::
	-rm -f baker/*.cm* baker/*~ baker/*.o baker/*.a
	-rm -f baker/*.deps baker/*.deps.byte
