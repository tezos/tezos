
-include Makefile.local
include Makefile.config

TZCOMPILER=../tezos-protocol-compiler
TZNODE=../tezos-node
TZCLIENT=../tezos-client
TZWEBCLIENT=../tezos-webclient
TZATTACKER=../tezos-attacker

all: ${TZCOMPILER} ${TZNODE} ${TZCLIENT} ${TZWEBCLIENT} ${TZATTACKER}


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
## Minimal utils library compatible with js_of_ocaml
############################################################################

MINUTILS_LIB_INTFS := \
  minutils/mBytes.mli \
	minutils/hex_encode.mli \
  minutils/utils.mli \
  minutils/compare.mli \
  minutils/data_encoding.mli \
  minutils/RPC.mli \

MINUTILS_LIB_IMPLS := \
  minutils/mBytes.ml \
  minutils/hex_encode.ml \
  minutils/utils.ml \
  minutils/compare.ml \
  minutils/data_encoding.ml \
  minutils/RPC.ml \

MINUTILS_PACKAGES := \
  cstruct \
  lwt  \
  ocplib-json-typed.bson \
  ocplib-resto.directory \
  $(COVERAGEPKG) \

MINUTILS_OBJS := \
  ${MINUTILS_LIB_IMPLS:.ml=.cmx} ${MINUTILS_LIB_IMPLS:.ml=.ml.deps} \
  ${MINUTILS_LIB_INTFS:.mli=.cmi} ${MINUTILS_LIB_INTFS:.mli=.mli.deps}
${MINUTILS_OBJS}: PACKAGES=${MINUTILS_PACKAGES}
${MINUTILS_OBJS}: SOURCE_DIRECTORIES=minutils
${MINUTILS_OBJS}: TARGET="(minutils.cmxa)"
${MINUTILS_OBJS}: OPENED_MODULES=

minutils.cmxa: ${MINUTILS_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

MINUTILS_OBJS_BYTECODE := \
  ${MINUTILS_LIB_IMPLS:.ml=.cmo} ${MINUTILS_LIB_IMPLS:.ml=.ml.deps.byte}
${MINUTILS_OBJS_BYTECODE}: PACKAGES=${MINUTILS_PACKAGES}
${MINUTILS_OBJS_BYTECODE}: SOURCE_DIRECTORIES=minutils
${MINUTILS_OBJS_BYTECODE}: TARGET="(minutils.cma)"
${MINUTILS_OBJS_BYTECODE}: OPENED_MODULES=

minutils.cma: ${MINUTILS_LIB_IMPLS:.ml=.cmo}
	@echo LINK $(notdir $@)
	@${OCAMLC} ${OCAMLFLAGS} -a -o $@ $^

############################################################################
## Utils library
############################################################################

UTILS_LIB_INTFS := \
  utils/base48.mli \
  utils/cli_entries.mli \
  utils/data_encoding_ezjsonm.mli \
  utils/crypto_box.mli \
  utils/time.mli \
  utils/hash.mli \
  utils/error_monad.mli \
  utils/lwt_exit.mli \
  utils/logging.mli \
  utils/lwt_utils.mli \
  utils/lwt_pipe.mli \
  utils/IO.mli \
  utils/moving_average.mli \
  utils/ring.mli \

UTILS_LIB_IMPLS := \
  utils/base48.ml \
  utils/cli_entries.ml \
  utils/data_encoding_ezjsonm.ml \
  utils/time.ml \
  utils/hash.ml \
  utils/crypto_box.ml \
  utils/error_monad_sig.ml \
  utils/error_monad.ml \
  utils/lwt_exit.ml \
  utils/logging.ml \
  utils/lwt_utils.ml \
  utils/lwt_pipe.ml \
  utils/IO.ml \
  utils/moving_average.ml \
  utils/ring.ml \

UTILS_PACKAGES := \
  ${MINUTILS_PACKAGES} \
  base64 \
  calendar \
  ezjsonm \
  sodium \
  zarith \
  $(COVERAGEPKG) \

UTILS_OBJS := \
  ${UTILS_LIB_IMPLS:.ml=.cmx} ${UTILS_LIB_IMPLS:.ml=.ml.deps} \
  ${UTILS_LIB_INTFS:.mli=.cmi} ${UTILS_LIB_INTFS:.mli=.mli.deps}
${UTILS_OBJS}: PACKAGES=${UTILS_PACKAGES}
${UTILS_OBJS}: SOURCE_DIRECTORIES=minutils utils
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
${COMPILER_OBJS}: SOURCE_DIRECTORIES=utils minutils compiler
${COMPILER_OBJS}: TARGET="(compiler.cmxa)"
${COMPILER_OBJS}: \
  OPENED_MODULES=Error_monad Hash Utils

compiler.cmxa: ${COMPILER_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

${TZCOMPILER}: minutils.cmxa utils.cmxa compiler.cmxa ${COMPILER_IMPLS:.ml=.cmx}
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
  node/net/RPC_server.mli \
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
  node/net/RPC_server.ml \
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
  cmdliner \


EMBEDDED_NODE_PROTOCOLS := \
  $(patsubst proto/%/,proto/embedded_proto_%.cmxa, \
  $(filter-out proto/environment/,$(shell ls -d proto/*/)))

NODE_OBJS := \
  ${NODE_IMPLS:.ml=.cmx} ${NODE_IMPLS:.ml=.ml.deps} \
  ${NODE_LIB_IMPLS:.ml=.cmx} ${NODE_LIB_IMPLS:.ml=.ml.deps} \
  ${NODE_LIB_INTFS:.mli=.cmi} ${NODE_LIB_INTFS:.mli=.mli.deps} \
  ${TZNODE}
${NODE_OBJS}: PACKAGES=${NODE_PACKAGES}
${NODE_OBJS}: SOURCE_DIRECTORIES=minutils utils compiler node/db node/net node/updater node/shell
${NODE_OBJS}: TARGET="(node.cmxa)"
${NODE_OBJS}: OPENED_MODULES=Error_monad Hash Utils

node.cmxa: ${NODE_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

 ${NODE_IMPLS:.ml=.cmx}: ${EMBEDDED_CLIENT_PROTOCOLS}
${TZNODE}: minutils.cmxa utils.cmxa compiler.cmxa node.cmxa ${EMBEDDED_NODE_PROTOCOLS} ${NODE_IMPLS:.ml=.cmx}
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
	@${TZCOMPILER} -static ${DEVFLAGS} \
	    -build-dir proto/$*/_tzbuild \
	    $@ proto/$*/

CLIENT_PROTO_INCLUDES := \
  minutils utils node/updater node/db node/net node/shell client \
  $(shell ocamlfind query lwt ocplib-json-typed sodium)

proto/client_embedded_proto_%.cmxa: \
  ${TZCOMPILER} \
  node/updater/environment.cmi \
  node/updater/environment.cmx \
  proto/%/TEZOS_PROTOCOL \
  $$(wildcard proto/%/*.ml) \
  $$(wildcard proto/%/*.mli)
	@./${TZCOMPILER} -static -client ${DEVFLAGS} \
	   -build-dir client/embedded/$*/_tzbuild \
	   $(addprefix -I , ${CLIENT_PROTO_INCLUDES}) \
	   $@ proto/$*



clean::
	rm -f ${TZNODE}

############################################################################
## Client program
############################################################################

CLIENT_LIB_INTFS := \
  client/client_commands.mli \
  client/client_node_rpcs.mli \
  client/client_generic_rpcs.mli \
  client/client_helpers.mli \
  client/client_aliases.mli \
  client/client_keys.mli \
  client/client_protocols.mli \

CLIENT_LIB_IMPLS := \
  client/client_commands.ml \
  client/client_config.ml \
  client/client_node_rpcs.ml \
  client/client_generic_rpcs.ml \
  client/client_helpers.ml \
  client/client_aliases.ml \
  client/client_keys.ml \
  client/client_protocols.ml \

WEBCLIENT_LIB_INTFS := \

WEBCLIENT_LIB_IMPLS := \
  client/webclient_version.ml \

CLIENT_IMPLS := \
  client_main.ml

WEBCLIENT_IMPLS := \
  webclient_static.ml \
  webclient_main.ml

CLIENT_PACKAGES := \
  ${NODE_PACKAGES}

EMBEDDED_CLIENT_PROTOCOLS := \
  $(patsubst client/embedded/%/, \
	     proto/client_embedded_proto_%.cmxa, \
             $(shell ls -d client/embedded/*/))

EMBEDDED_CLIENT_VERSIONS := \
	$(patsubst client/embedded/%/, \
	     client/embedded/client_%.cmx, \
             $(shell ls -d client/embedded/*/))

EMBEDDED_WEBCLIENT_VERSIONS := \
	$(patsubst client/embedded/%/, \
	     client/embedded/webclient_%.cmx, \
             $(shell ls -d client/embedded/*/))

CLIENT_OBJS := \
  ${CLIENT_IMPLS:.ml=.cmx} ${CLIENT_IMPLS:.ml=.ml.deps} \
  ${WEBCLIENT_IMPLS:.ml=.cmx} ${WEBCLIENT_IMPLS:.ml=.ml.deps} \
  ${CLIENT_LIB_IMPLS:.ml=.cmx} ${CLIENT_LIB_IMPLS:.ml=.ml.deps} \
  ${CLIENT_LIB_INTFS:.mli=.cmi} ${CLIENT_LIB_INTFS:.mli=.mli.deps} \
  ${WEBCLIENT_LIB_IMPLS:.ml=.cmx} ${WEBCLIENT_LIB_IMPLS:.ml=.ml.deps} \
  ${WEBCLIENT_LIB_INTFS:.mli=.cmi} ${WEBCLIENT_LIB_INTFS:.mli=.mli.deps} \
  ${TZCLIENT} \
  ${TZWEBCLIENT}
${CLIENT_OBJS}: PACKAGES=${CLIENT_PACKAGES}
${CLIENT_OBJS}: SOURCE_DIRECTORIES=client client/embedded minutils utils node/net node/shell node/updater node/db compiler
${CLIENT_OBJS}: OPENED_MODULES=Error_monad Hash Utils

${CLIENT_LIB_IMPLS:.ml=.cmx} ${CLIENT_LIB_IMPLS:.ml=.ml.deps}: TARGET="(client.cmxa)"
${CLIENT_LIB_INTFS:.mli=.cmi} ${CLIENT_LIB_INTFS:.mli=.mli.deps}: TARGET="(client.cmxa)"
${WEBCLIENT_LIB_IMPLS:.ml=.cmx} ${WEBCLIENT_LIB_IMPLS:.ml=.ml.deps}: TARGET="(webclient.cmxa)"
${WEBCLIENT_LIB_INTFS:.mli=.cmi} ${WEBCLIENT_LIB_INTFS:.mli=.mli.deps}: TARGET="(webclient.cmxa)"

client.cmxa: ${CLIENT_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

webclient.cmxa: ${WEBCLIENT_LIB_IMPLS:.ml=.cmx} client.cmxa
	@echo LINK $(notdir $@)
	${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ ${WEBCLIENT_LIB_IMPLS:.ml=.cmx}

${EMBEDDED_CLIENT_VERSIONS}: client.cmxa
${CLIENT_IMPLS:.ml=.cmx}: ${EMBEDDED_CLIENT_VERSIONS}

${EMBEDDED_WEBCLIENT_VERSIONS}: webclient.cmxa
${WEBCLIENT_IMPLS:.ml=.cmx}: ${EMBEDDED_CLIENT_VERSIONS} ${EMBEDDED_WEBCLIENT_VERSIONS}

${TZCLIENT}: minutils.cmxa utils.cmxa compiler.cmxa node.cmxa \
             client.cmxa \
             ${EMBEDDED_CLIENT_PROTOCOLS} \
             ${EMBEDDED_CLIENT_VERSIONS} \
             ${CLIENT_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkpkg ${OCAMLFLAGS} -o $@ $^

${TZWEBCLIENT}: minutils.cmxa utils.cmxa compiler.cmxa node.cmxa \
             client.cmxa webclient.cmxa \
             ${EMBEDDED_CLIENT_PROTOCOLS} \
             ${EMBEDDED_CLIENT_VERSIONS} \
             ${EMBEDDED_WEBCLIENT_VERSIONS} \
             ${WEBCLIENT_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkpkg ${OCAMLFLAGS} -o $@ $^

webclient_static.ml: $$(shell find  webclient_static/)
	ocp-ocamlres webclient_static -o $@

clean::
	-rm -f ${TZCLIENT} $(TZWEBCLIENT)

## Embedded client protocol modules

.SECONDEXPANSION:

client/embedded/client_%.cmx: \
  $(patsubst %.ml, %.cmx, $(NODE_LIB_IMPLS) $(CLIENT_LIB_IMPLS)) \
  proto/client_embedded_proto_%.cmxa \
  $$(shell find client/embedded/% -name \*.ml -or -name \*.mli)
	@$(MAKE) -C client/embedded/$* ../client_$*.cmx

client/embedded/webclient_%.cmx: \
  client/embedded/client_%.cmx \
	minutils.cma \
  $$(shell find client/embedded/%/webclient -name \*.ml -or -name \*.mli) \
  $$(shell find client/embedded/%/webclient/static/)
	@$(MAKE) -C client/embedded/$* ../webclient_$*.cmx

clean::
	-for d in $$(ls -d client/embedded/*/) ; do make clean -C $$d ; done
	-rm -f client/embedded/*.cm* client/embedded/*.o


############################################################################
## Attacker program
############################################################################

ATTACKER_LIB_INTFS := \
  attacker/attacker_minimal.mli \

ATTACKER_LIB_IMPLS := \
  attacker/attacker_minimal.ml \

ATTACKER_IMPLS := \
  attacker_main.ml

ATTACKER_PACKAGES := \
  ${NODE_PACKAGES}

EMBEDDED_ATTACKER_PROTOCOLS := \
  $(patsubst client/embedded/%/, \
	     proto/client_embedded_proto_%.cmxa, \
             $(shell ls -d client/embedded/*/))

ATTACKER_OBJS := \
  ${ATTACKER_IMPLS:.ml=.cmx} ${ATTACKER_IMPLS:.ml=.ml.deps} \
  ${ATTACKER_LIB_IMPLS:.ml=.cmx} ${ATTACKER_LIB_IMPLS:.ml=.ml.deps} \
  ${ATTACKER_LIB_INTFS:.mli=.cmi} ${ATTACKER_LIB_INTFS:.mli=.mli.deps} \
  ${TZATTACKER}
${ATTACKER_OBJS}: PACKAGES=${ATTACKER_PACKAGES}
${ATTACKER_OBJS}: SOURCE_DIRECTORIES=attacker proto client/embedded minutils utils node/net node/shell node/updater node/db compiler
${ATTACKER_OBJS}: TARGET="(attacker.cmxa)"
${ATTACKER_OBJS}: OPENED_MODULES=Error_monad Hash Utils

attacker.cmxa: ${ATTACKER_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

${ATTACKER_LIB_IMPLS:.ml=.cmx} ${ATTACKER_IMPLS:.ml=.cmx}: \
  ${EMBEDDED_ATTACKER_PROTOCOLS}

${TZATTACKER}: minutils.cmxa utils.cmxa compiler.cmxa node.cmxa \
	       ${EMBEDDED_ATTACKER_PROTOCOLS} attacker.cmxa \
	     ${ATTACKER_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	-rm -f ${TZATTACKER}



############################################################################
## Generic rules
############################################################################

%.cmx: %.ml
	@echo OCAMLOPT ${TARGET} $(notdir $@)
	@$(OCAMLOPT) ${OCAMLFLAGS} -c $<

%.cmo: %.ml
	@echo OCAMLC ${TARGET} $(notdir $@)
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
include .depend
endif
endif
DEPENDS := $(filter-out $(NO_DEPS), \
  $(MINUTILS_LIB_INTFS) $(MINUTILS_LIB_IMPLS) \
  $(UTILS_LIB_INTFS) $(UTILS_LIB_IMPLS) \
  $(COMPILER_LIB_INTFS) $(COMPILER_LIB_IMPLS) \
  $(COMPILER_INTFS) $(COMPILER_IMPLS) \
  $(NODE_LIB_INTFS) $(NODE_LIB_IMPLS) \
  $(NODE_INTFS) $(NODE_IMPLS) \
  $(CLIENT_LIB_INTFS) $(CLIENT_LIB_IMPLS) \
  $(CLIENT_INTFS) $(CLIENT_IMPLS) \
  $(WEBCLIENT_LIB_INTFS) $(WEBCLIENT_LIB_IMPLS) \
  $(ATTACKER_LIB_INTFS) $(ATTACKER_LIB_IMPLS) \
  $(WEBCLIENT_INTFS) $(WEBCLIENT_IMPLS) \
  $(ATTACKER_INTFS) $(ATTACKER_IMPLS) \
)
DEPENDS_BYTECODE := \
  $(filter-out $(NO_DEPS), $(MINUTILS_LIB_INTFS) $(MINUTILS_LIB_IMPLS))

predepend: node/updater/proto_environment.mli
compiler/tezos_compiler.cmo compiler/tezos_compiler.cmx: \
	compiler/embedded_cmis.cmi compiler/embedded_cmis.cmx

.SECONDARY: $(patsubst %,%.deps,${DEPENDS}) $(patsubst %,%.deps.byte,${DEPENDS_BYTECODE})
.depend: $(patsubst %,%.deps,${DEPENDS}) $(patsubst %,%.deps.byte,${DEPENDS_BYTECODE})
	@cat $^ > .depend
%.ml.deps: %.ml | predepend
	@echo OCAMLDEP ${TARGET} $(notdir $^)
	@$(OCAMLDEP) -native $(INCLUDES) $^ > $@
%.mli.deps: %.mli | predepend
	@echo OCAMLDEP ${TARGET} $(notdir $^)
	@$(OCAMLDEP) -native $(INCLUDES) $^ > $@
%.ml.deps.byte: %.ml | predepend
	@echo OCAMLDEP ${TARGET} $(notdir $^)
	@$(OCAMLDEP) $(INCLUDES) $^ > $@
%.mli.deps.byte: %.mli | predepend
	@echo OCAMLDEP ${TARGET} $(notdir $^)
	@$(OCAMLDEP) $(INCLUDES) $^ > $@

clean::
	-rm -f .depend
	-find \( -name \*.mli.deps -or -name \*.ml.deps \) -delete
