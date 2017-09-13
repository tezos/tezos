
PROTO_VERSION := alpha

CLIENT_INTFS := \
  concrete_parser.mli \
  client_proto_rpcs.mli \
  client_proto_args.mli \
  client_proto_contracts.mli \
  client_proto_programs.mli \
  client_proto_context.mli \
  client_proto_nonces.mli \
  client_proto_main.mli

CLIENT_IMPLS := \
  script_located_ir.ml \
  concrete_parser.ml concrete_lexer.ml \
  client_proto_rpcs.ml \
  client_proto_args.ml \
  client_proto_contracts.ml \
  client_proto_programs.ml \
  client_proto_context.ml \
  client_proto_nonces.ml \
  client_proto_main.ml

-include baker/Makefile

include ../Makefile.shared

${OBJS}: OPENED_MODULES += Environment Tezos_context

predepend: concrete_parser.ml concrete_lexer.ml

.PHONY: clean
clean::
	-rm -f baker/*.cm* baker/*~ baker/*.o baker/*.a
	-rm -f baker/*.deps baker/*.deps.byte
	-rm -f concrete_lexer.ml concrete_parser.ml concrete_parser.mli
