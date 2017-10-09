
PROTO_VERSION = demo

CLIENT_IMPLS = \
  client_proto_demo.ml \
  client_proto_rpcs.ml \
  client_proto_main.ml

CLIENT_INTFS = \
  client_proto_rpcs.mli \
  client_proto_main.mli

include ../Makefile.shared

${OBJS_DEPS} ${OBJS}: OPENED_MODULES += Client_proto_demo
client_proto_demo.ml.deps client_proto_demo.cmx: OPENED_MODULES :=
