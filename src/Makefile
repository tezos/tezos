
include Makefile.config

TZCOMPILER=../tezos-protocol-compiler
TZNODE=../tezos-node
TZCLIENT=../tezos-client

all: ${TZCOMPILER} ${TZNODE} ${TZCLIENT}


############################################################################
## Protocol environment
############################################################################

PROTOCOL_ENV_INTFS := \
$(addprefix proto/environment/, \
  pervasives.mli \
  compare.mli \
  \
  array.mli list.mli bytes.mli string.mli \
  set.mli map.mli \
  int32.mli int64.mli \
  buffer.mli \
  format.mli \
  \
  lwt_sequence.mli lwt.mli lwt_list.mli \
  \
  mBytes.mli \
  hex_encode.mli \
  \
  uri.mli \
  data_encoding.mli \
  error_monad.mli \
  logging.mli \
  time.mli \
  base48.mli \
  hash.mli \
  ed25519.mli \
  persist.mli \
  context.mli \
  RPC.mli \
  \
  fitness.mli \
  updater.mli \
)

.INTERMEDIATE: node/updater/environment_gen
.SECONDARY: node/updater/proto_environment.mli

node/updater/environment_gen: node/updater/environment_gen.ml
	@echo LINK $(notdir $@)
	@$(OCAMLOPT) -o $@ $^

node/updater/proto_environment.mli: \
	node/updater/environment_gen $(PROTOCOL_ENV_INTFS)
	@echo GENERATING $(notdir $@)
	@node/updater/environment_gen node/updater/proto_environment.mli \
	  $(PROTOCOL_ENV_INTFS)

node/updater/proto_environment.cmi: \
  node/updater/proto_environment.mli node/updater/protocol.cmi
	@echo OCAMLOPT ${TARGET} $@
	@$(OCAMLOPT) -nopervasives -nostdlib -opaque -I tmp -I node/updater -c $<

clean::
	rm -f node/updater/proto_environment.mli
	rm -f node/updater/environment_gen

############################################################################
## Protocol environment
############################################################################


EMBEDDED_PROTOCOL_LIB_CMIS := \
  tmp/camlinternalFormatBasics.cmi \
  node/updater/proto_environment.cmi \
  node/updater/register.cmi

node/updater/register.cmi: EXTRA_OCAMLFLAGS = -opaque
node/updater/environment.cmi: node/updater/environment.cmx

tmp/camlinternalFormatBasics.cmi:
	mkdir -p tmp
	ln -sf $(shell ocamlc -where)/camlinternalFormatBasics.cmi $@

.INTERMEDIATE: compiler/embedded_cmis.ml
compiler/embedded_cmis.cmx: compiler/embedded_cmis.cmi
compiler/embedded_cmis.ml: ${EMBEDDED_PROTOCOL_LIB_CMIS}
	@echo OCAMLRES ${TARGET} $(notdir $@)
	@$(OCAMLRES) -format ocaml -o $@ $^
clean::
	rm -f compiler/embedded_cmis.ml
	rm -rf tmp


############################################################################
## Node protocol compiler (also embedded in the main program)
############################################################################

UTILS_LIB_INTFS := \
  utils/mBytes.mli \
  utils/utils.mli \
  utils/base48.mli \
  utils/hex_encode.mli \
  utils/cli_entries.mli \
  utils/compare.mli \
  utils/data_encoding.mli \
  utils/time.mli \
  utils/hash.mli \
  utils/error_monad.mli \
  utils/logging.mli \
  utils/lwt_utils.mli \
  utils/IO.mli \

UTILS_LIB_IMPLS := \
  utils/mBytes.ml \
  utils/utils.ml \
  utils/hex_encode.ml \
  utils/base48.ml \
  utils/cli_entries.ml \
  utils/compare.ml \
  utils/data_encoding.ml \
  utils/time.ml \
  utils/hash.ml \
  utils/error_monad_sig.ml \
  utils/error_monad.ml \
  utils/logging.ml \
  utils/lwt_utils.ml \
  utils/IO.ml \

UTILS_PACKAGES := \
  base64 \
  calendar \
  cryptokit \
  cstruct \
  ezjsonm \
  lwt  \
  ocplib-json-typed \
  sodium \
  zarith \
  $(COVERAGEPKG) \

UTILS_OBJS := \
  ${UTILS_LIB_IMPLS:.ml=.cmx} ${UTILS_LIB_IMPLS:.ml=.ml.deps} \
  ${UTILS_LIB_INTFS:.mli=.cmi} ${UTILS_LIB_INTFS:.mli=.mli.deps}
${UTILS_OBJS}: PACKAGES=${UTILS_PACKAGES}
${UTILS_OBJS}: SOURCE_DIRECTORIES=utils
${UTILS_OBJS}: TARGET="(utils.cmxa)"
${UTILS_OBJS}: OPENED_MODULES=

utils.cmxa: ${UTILS_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^


############################################################################
## Node protocol compiler (also embedded in the main program)
############################################################################

COMPILER_LIB_INTFS := \
  compiler/tezos_compiler.mli \
  compiler/embedded_cmis.mli \

COMPILER_LIB_IMPLS := \
  compiler/embedded_cmis.ml \
  compiler/tezos_compiler.ml \

COMPILER_IMPLS := \
  compiler_main.ml \

COMPILER_PACKAGES := \
  ${UTILS_PACKAGES} \
  compiler-libs.optcomp \
  config-file \
  lwt.unix \
  ocplib-endian \
  ocplib-ocamlres \
  unix \

COMPILER_OBJS := \
  ${COMPILER_IMPLS:.ml=.cmx} ${COMPILER_IMPLS:.ml=.ml.deps} \
  ${COMPILER_LIB_IMPLS:.ml=.cmx} ${COMPILER_LIB_IMPLS:.ml=.ml.deps} \
  ${COMPILER_LIB_INTFS:.mli=.cmi} ${COMPILER_LIB_INTFS:.mli=.mli.deps} \
  ${TZCOMPILER}
${COMPILER_OBJS}: PACKAGES=${COMPILER_PACKAGES}
${COMPILER_OBJS}: SOURCE_DIRECTORIES=utils compiler
${COMPILER_OBJS}: TARGET="(compiler.cmxa)"
${COMPILER_OBJS}: \
  OPENED_MODULES=Error_monad Hash Utils

compiler.cmxa: ${COMPILER_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

${TZCOMPILER}: utils.cmxa compiler.cmxa ${COMPILER_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@$(OCAMLOPT) -linkpkg $(patsubst %, -package %, $(COMPILER_PACKAGES)) -o $@ $^

clean::
	rm -f ${TZCOMPILER}


############################################################################
## Node program
############################################################################

NODE_LIB_INTFS := \
  \
  node/net/p2p.mli \
  node/net/RPC.mli \
  \
  node/updater/fitness.mli \
  \
  node/db/ir_funview.mli \
  node/db/persist.mli \
  node/db/context.mli \
  node/db/store.mli \
  node/db/db_proxy.mli \
  \
  node/updater/protocol.mli \
  node/updater/updater.mli \
  node/updater/proto_environment.mli \
  node/updater/register.mli \
  \
  node/shell/tezos_p2p.mli \
  node/shell/state.mli \
  node/shell/prevalidator.mli \
  node/shell/validator.mli \
  \
  node/shell/discoverer.mli \
  node/shell/node_rpc_services.mli \
  node/shell/node.mli \
  node/shell/node_rpc.mli \

NODE_LIB_IMPLS := \
  \
  compiler/node_compiler_main.ml \
  \
  node/net/p2p.ml \
  node/net/RPC.ml \
  \
  node/updater/fitness.ml \
  \
  node/db/ir_funview.ml \
  node/db/persist.ml \
  node/db/store.ml \
  node/db/context.ml \
  node/db/db_proxy.ml \
  \
  node/updater/updater.ml \
  node/updater/environment.ml \
  node/updater/proto_environment.ml \
  node/updater/register.ml \
  \
  node/shell/tezos_p2p.ml \
  node/shell/state.ml \
  node/shell/prevalidator.ml \
  node/shell/validator.ml \
  \
  node/shell/discoverer.ml \
  node/shell/node_rpc_services.ml \
  node/shell/node.ml \
  node/shell/node_rpc.ml \

NODE_IMPLS := \
  node_main.ml \

NODE_PACKAGES := \
  $(COMPILER_PACKAGES) \
  calendar \
  cohttp.lwt \
  dynlink \
  git \
  irmin.unix \
  ocplib-resto.directory \


EMBEDDED_NODE_PROTOCOLS := \
  $(patsubst proto/%/,proto/embedded_proto_%.cmxa, \
  $(filter-out proto/environment/,$(shell ls -d proto/*/)))

NODE_OBJS := \
  ${NODE_IMPLS:.ml=.cmx} ${NODE_IMPLS:.ml=.ml.deps} \
  ${NODE_LIB_IMPLS:.ml=.cmx} ${NODE_LIB_IMPLS:.ml=.ml.deps} \
  ${NODE_LIB_INTFS:.mli=.cmi} ${NODE_LIB_INTFS:.mli=.mli.deps} \
  ${TZNODE}
${NODE_OBJS}: PACKAGES=${NODE_PACKAGES}
${NODE_OBJS}: SOURCE_DIRECTORIES=utils compiler node/db node/net node/updater node/shell
${NODE_OBJS}: TARGET="(node.cmxa)"
${NODE_OBJS}: OPENED_MODULES=Error_monad Hash Utils

node.cmxa: ${NODE_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

 ${NODE_IMPLS:.ml=.cmx}: ${EMBEDDED_CLIENT_PROTOCOLS}
${TZNODE}: utils.cmxa compiler.cmxa node.cmxa ${EMBEDDED_NODE_PROTOCOLS} ${NODE_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	-rm -f proto/*.cm* proto/*.a

## Embedded protocol modules

.SECONDEXPANSION:
proto/embedded_proto_%.cmxa: \
  ${TZCOMPILER} \
  proto/%/TEZOS_PROTOCOL \
  $$(wildcard proto/%/*.ml) \
  $$(wildcard proto/%/*.mli)
	@${TZCOMPILER} -static -build-dir proto/$*/_tzbuild $@ proto/$*/

CLIENT_PROTO_INCLUDES := \
  utils node/updater node/db node/net node/shell client \
  $(shell ocamlfind query lwt ocplib-json-typed sodium)

proto/client_embedded_proto_%.cmxa: \
  ${TZCOMPILER} \
  node/updater/environment.cmi \
  node/updater/environment.cmx \
  proto/%/TEZOS_PROTOCOL \
  $$(wildcard proto/%/*.ml) \
  $$(wildcard proto/%/*.mli)
	@./${TZCOMPILER} -static -client -build-dir client/embedded/$*/_tzbuild \
	   $(addprefix -I , ${CLIENT_PROTO_INCLUDES}) \
	   $@ proto/$*



clean::
	rm -f ${TZNODE}


############################################################################
## Client program
############################################################################

CLIENT_LIB_INTFS := \
  client/client_version.mli \
  client/client_node_rpcs.mli \
  client/client_generic_rpcs.mli \
  client/client_helpers.mli \
  client/client_aliases.mli \
  client/client_keys.mli \
  client/client_protocols.mli \

CLIENT_LIB_IMPLS := \
  client/client_version.ml \
  client/client_config.ml \
  client/client_node_rpcs.ml \
  client/client_generic_rpcs.ml \
  client/client_helpers.ml \
  client/client_aliases.ml \
  client/client_keys.ml \
  client/client_protocols.ml \

CLIENT_IMPLS := \
  client_main.ml

CLIENT_PACKAGES := \
  ${NODE_PACKAGES}

EMBEDDED_CLIENT_PROTOCOLS := \
  $(patsubst client/embedded/%/, \
	     proto/client_embedded_proto_%.cmxa, \
             $(shell ls -d client/embedded/*/)) \
  $(patsubst client/embedded/%/, \
	     client/embedded/client_%.cmx , \
             $(shell ls -d client/embedded/*/))


CLIENT_OBJS := \
  ${CLIENT_IMPLS:.ml=.cmx} ${CLIENT_IMPLS:.ml=.ml.deps} \
  ${CLIENT_LIB_IMPLS:.ml=.cmx} ${CLIENT_LIB_IMPLS:.ml=.ml.deps} \
  ${CLIENT_LIB_INTFS:.mli=.cmi} ${CLIENT_LIB_INTFS:.mli=.mli.deps} \
  ${TZCLIENT}
${CLIENT_OBJS}: PACKAGES=${CLIENT_PACKAGES}
${CLIENT_OBJS}: SOURCE_DIRECTORIES=client client/embedded utils node/net node/shell node/updater node/db compiler
${CLIENT_OBJS}: TARGET="(client.cmxa)"
${CLIENT_OBJS}: OPENED_MODULES=Error_monad Hash Utils

client.cmxa: ${CLIENT_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

${EMBEDDED_CLIENT_PROTOCOLS}: client.cmxa
${CLIENT_IMPLS:.ml=.cmx}: ${EMBEDDED_CLIENT_PROTOCOLS}

${TZCLIENT}: utils.cmxa compiler.cmxa node.cmxa \
             client.cmxa ${EMBEDDED_CLIENT_PROTOCOLS} \
	     ${CLIENT_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	-rm -f ${TZCLIENT}

## Embedded client protocol modules

.SECONDEXPANSION:
client/embedded/client_%.cmx: \
  $(patsubst %.ml, %.cmx, $(NODE_LIB_IMPLS) $(CLIENT_LIB_IMPLS)) \
  proto/client_embedded_proto_%.cmxa \
  $$(shell find client/embedded/% -name \*.ml -or -name \*.mli)
	@$(MAKE) -C client/embedded/$* ../client_$*.cmx
clean::
	-for d in $$(ls -d client/embedded/*/) ; do make clean -C $$d ; done
	-rm -f client/embedded/*.cm* client/embedded/*.o

## Generic rules

%.cmx: %.ml
	@echo OCAMLOPT ${TARGET} $(notdir $@)
	@$(OCAMLOPT) ${OCAMLFLAGS} -c $<

%.cmo: %.ml
	@echo OCAMLOPT ${TARGET} $(notdir $@)
	@$(OCAMLC) ${OCAMLFLAGS} -c $<

%.cmi: %.mli
	@echo OCAMLOPT ${TARGET} $(notdir $@)
	@$(OCAMLOPT) ${OCAMLFLAGS} -c $<

## Cleaning

.PHONY: clean
clean::
	-find \( -name \*.cm\* -or -name \*.cmp -or -name \*.out -or -name \*~ -or -name \*.o -or -name \*.a \) -delete

## Dependencies

NO_DEPS := \
   compiler/embedded_cmis.ml \
   compiler/embedded_cmis.mli
compiler/embedded_cmis.cmx compiler/embedded_cmis.cmi: OPENED_MODULES=

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),build-deps)
-include .depend
endif
endif
DEPENDS := $(filter-out $(NO_DEPS), $(UTILS_LIB_INTFS) $(UTILS_LIB_IMPLS) \
                                    $(COMPILER_LIB_INTFS) $(COMPILER_LIB_IMPLS) \
                                    $(COMPILER_INTFS) $(COMPILER_IMPLS) \
                                    $(NODE_LIB_INTFS) $(NODE_LIB_IMPLS) \
                                    $(NODE_INTFS) $(NODE_IMPLS) \
                                    $(CLIENT_LIB_INTFS) $(CLIENT_LIB_IMPLS) \
                                    $(CLIENT_INTFS) $(CLIENT_IMPLS))

predepend: node/updater/proto_environment.mli
compiler/tezos_compiler.cmo compiler/tezos_compiler.cmx: \
	compiler/embedded_cmis.cmi compiler/embedded_cmis.cmx

.SECONDARY: $(patsubst %,%.deps,${DEPENDS})
.depend: $(patsubst %,%.deps,${DEPENDS})
	@cat $^ > .depend
%.ml.deps: %.ml | predepend
	@echo OCAMLDEP ${TARGET} $(notdir $^)
	@$(OCAMLDEP) -native $(INCLUDES) $^ > $@
%.mli.deps: %.mli | predepend
	@echo OCAMLDEP ${TARGET} $(notdir $^)
	@$(OCAMLDEP) -native $(INCLUDES) $^ > $@

clean::
	-rm -f .depend
	-find \( -name \*.mli.deps -or -name \*.ml.deps \) -delete
