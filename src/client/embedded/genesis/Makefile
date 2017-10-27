
PROTO_VERSION = genesis

CLIENT_IMPLS = \
  client_proto_genesis.ml \
  client_proto_main.ml

CLIENT_INTFS = \
  client_proto_main.mli

include ../Makefile.shared

SOURCE_DIRECTORIES += ..

${OBJS_DEPS} ${OBJS}: ../client_embedded_alpha.cmx
${OBJS_DEPS} ${OBJS}: OPENED_MODULES += Client_proto_genesis
client_proto_genesis.ml.deps client_proto_genesis.cmx: OPENED_MODULES :=

../client_embedded_alpha.cmx:
