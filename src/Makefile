
-include Makefile.local
include Makefile.config

TZCOMPILER=../tezos-protocol-compiler
TZNODE=../tezos-node
TZCLIENT=../tezos-client
TZWEBCLIENT=../tezos-webclient
TZATTACKER=../tezos-attacker

all: ${TZCOMPILER} ${TZNODE} ${TZCLIENT} ${TZWEBCLIENT} # ${TZATTACKER}


NODEPS :=

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
  base58.mli \
  hash.mli \
  ed25519.mli \
  persist.mli \
  fitness.mli \
  context.mli \
  RPC.mli \
  \
  updater.mli \
)

.INTERMEDIATE: compiler/environment_gen
.SECONDARY: node/updater/proto_environment.mli

compiler/environment_gen: compiler/environment_gen.ml
	@echo LINK $(notdir $@)
	@$(OCAMLOPT) -o $@ $^

node/updater/proto_environment.mli: \
	compiler/environment_gen $(PROTOCOL_ENV_INTFS)
	@echo GENERATING $(notdir $@)
	@compiler/environment_gen node/updater/proto_environment.mli \
	  $(PROTOCOL_ENV_INTFS)

compiler/sigs/proto_environment.mli: node/updater/proto_environment.mli

compiler/sigs/proto_environment.cmi: \
  compiler/sigs/proto_environment.mli compiler/sigs/protocol.cmi \
  compiler/sigs/camlinternalFormatBasics.cmi
	@echo OCAMLOPT ${TARGET} $@
	@$(OCAMLOPT) -nopervasives -nostdlib -opaque -I tmp -I compiler/sigs -c $<

partial-clean::
	rm -f node/updater/proto_environment.mli
	rm -f compiler/environment_gen

############################################################################
## Protocol environment
############################################################################


COMPILER_EMBEDDED_CMIS := \
  compiler/sigs/camlinternalFormatBasics.cmi \
  compiler/sigs/proto_environment.cmi \
  compiler/sigs/register.cmi

compiler/sigs/register.cmi: EXTRA_OCAMLFLAGS = -opaque

COMPILER_PRECOMPILED_INTFS := \
  compiler/sigs/tezos_compiler.mli \
  compiler/sigs/fitness.mli \
  compiler/sigs/persist.mli \
  compiler/sigs/store_sigs.mli \
  compiler/sigs/store.mli \
  compiler/sigs/context.mli \
  compiler/sigs/protocol.mli \
  compiler/sigs/proto_environment.mli \
  compiler/sigs/register.mli

COMPILER_PRECOMPILED_OBJS := \
  ${COMPILER_PRECOMPILED_INTFS:.mli=.cmi} \
  ${COMPILER_PRECOMPILED_INTFS:.mli=.mli.deps}

${COMPILER_PRECOMPILED_OBJS}: PACKAGES=${MINUTILS_PACKAGES} ${UTILS_PACKAGES}
${COMPILER_PRECOMPILED_OBJS}: SOURCE_DIRECTORIES=minutils utils compiler/sigs/
${COMPILER_PRECOMPILED_OBJS}: TARGET="(embedded_cmis.cmx)"
${COMPILER_PRECOMPILED_OBJS}: OPENED_MODULES=Error_monad Hash Utils

compiler/sigs/camlinternalFormatBasics.cmi:
	ln -sf $(shell ocamlc -where)/camlinternalFormatBasics.cmi $@

.INTERMEDIATE: compiler/embedded_cmis.ml
compiler/embedded_cmis.cmx: compiler/embedded_cmis.cmi

compiler/embedded_cmis.ml: ${COMPILER_EMBEDDED_CMIS}
	@echo OCAMLRES ${TARGET} $(notdir $@)
	@$(OCAMLRES) -format ocaml -o $@ $^

partial-clean::
	rm -f compiler/embedded_cmis.ml

compiler/tezos_compiler.cmi: compiler/sigs/tezos_compiler.cmi
	@cp -a compiler/sigs/tezos_compiler.cmi compiler

NO_DEPS += \
  node/updater/fitness.mli \
  node/updater/protocol.mli \
  node/updater/proto_environment.mli \
  node/updater/register.mli \
  node/db/persist.mli \
  node/db/store_sigs.mli \
  node/db/store_sigs.mli \
  node/db/store.mli \
  node/db/context.mli

node/updater/fitness.cmi: compiler/sigs/fitness.cmi
	@cp -a compiler/sigs/fitness.cmi node/updater
node/updater/protocol.cmi: compiler/sigs/protocol.cmi
	@cp -a compiler/sigs/protocol.cmi node/updater
node/updater/proto_environment.cmi: compiler/sigs/proto_environment.cmi
	@cp -a compiler/sigs/proto_environment.cmi node/updater
node/updater/register.cmi: compiler/sigs/register.cmi
	@cp -a compiler/sigs/register.cmi node/updater

node/db/persist.cmi: compiler/sigs/persist.cmi
	@cp -a compiler/sigs/persist.cmi node/db
node/db/store_sigs.cmi: compiler/sigs/store_sigs.cmi
	@cp -a compiler/sigs/store_sigs.cmi node/db
node/db/store.cmi: compiler/sigs/store.cmi
	@cp -a compiler/sigs/store.cmi node/db
node/db/context.cmi: compiler/sigs/context.cmi
	@cp -a compiler/sigs/context.cmi node/db


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
  utils/base58.mli \
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
  utils/watcher.mli \

UTILS_LIB_IMPLS := \
  utils/base58.ml \
  utils/cli_entries.ml \
  utils/error_monad_sig.ml \
  utils/error_monad.ml \
  utils/data_encoding_ezjsonm.ml \
  utils/time.ml \
  utils/hash.ml \
  utils/crypto_box.ml \
  utils/lwt_exit.ml \
  utils/logging.ml \
  utils/lwt_utils.ml \
  utils/lwt_pipe.ml \
  utils/IO.ml \
  utils/moving_average.ml \
  utils/ring.ml \
  utils/watcher.ml \

UTILS_PACKAGES := \
  ${MINUTILS_PACKAGES} \
  base64 \
  calendar \
  ezjsonm \
  ipaddr.unix \
  lwt.unix \
  mtime.os \
  nocrypto \
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

utils.top: DEVFLAGS=
utils.top:
	( $(patsubst %, echo "#require \"%\";;" ; ,${UTILS_PACKAGES}) ) > .ocamlinit.utils
	( $(patsubst %, echo "#mod_use \"%\";;" ; ,${MINUTILS_LIB_IMPLS} ${UTILS_LIB_IMPLS}) ) >> .ocamlinit.utils
	utop -init .ocamlinit.utils ${OCAMLFLAGS}

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
  node/net/p2p_types.mli \
  node/net/p2p_io_scheduler.mli \
  node/net/p2p_connection.mli \
  node/net/p2p_connection_pool_types.mli \
  node/net/p2p_connection_pool.mli \
  node/net/p2p_welcome.mli \
  node/net/p2p_discovery.mli \
  node/net/p2p_maintenance.mli \
  node/net/p2p.mli \
  node/net/RPC_server.mli \
  \
  node/updater/fitness.mli \
  \
  node/db/store_sigs.mli \
  node/db/raw_store.mli \
  node/db/store_sigs.mli \
  node/db/store_helpers.mli \
  node/db/store.mli \
  \
  node/db/ir_funview.mli \
  node/db/persist.mli \
  node/db/context.mli \
  \
  node/updater/protocol.mli \
  node/updater/updater.mli \
  node/updater/proto_environment.mli \
  node/updater/register.mli \
  \
  node/shell/state.mli \
  node/shell/distributed_db_functors.mli \
  node/shell/distributed_db_message.mli \
  node/shell/distributed_db_metadata.mli \
  node/shell/distributed_db.mli \
  node/shell/prevalidator.mli \
  node/shell/validator.mli \
  \
  node/shell/node_rpc_services.mli \
  node/shell/node.mli \
  node/shell/node_rpc.mli \

FULL_NODE_LIB_IMPLS := \
  \
  compiler/node_compiler_main.ml \
  \
  node/net/p2p_types.ml \
  node/net/p2p_io_scheduler.ml \
  node/net/p2p_connection.ml \
  node/net/p2p_connection_pool_types.ml \
  node/net/p2p_connection_pool.ml \
  node/net/p2p_welcome.ml \
  node/net/p2p_discovery.ml \
  node/net/p2p_maintenance.ml \
  node/net/p2p.ml \
  \
  node/net/RPC_server.ml \
  \
  node/updater/fitness.ml \
  \
  node/db/raw_store.ml \
  node/db/store_sigs.mli \
  node/db/store_helpers.ml \
  node/db/store.ml \
  \
  node/db/ir_funview.ml \
  node/db/persist.ml \
  node/db/context.ml \
  \
  node/updater/protocol.mli \
  node/updater/updater.ml \
  node/updater/environment.ml \
  node/updater/proto_environment.ml \
  node/updater/register.ml \
  \
  node/shell/state.ml \
  node/shell/distributed_db_functors.ml \
  node/shell/distributed_db_message.ml \
  node/shell/distributed_db_metadata.ml \
  node/shell/distributed_db.ml \
  node/shell/prevalidator.ml \
  node/shell/validator.ml \
  \
  node/shell/node_rpc_services.ml \
  node/shell/node.ml \
  node/shell/node_rpc.ml \

NODE_LIB_IMPLS := $(filter-out ${NODE_LIB_INTFS}, ${FULL_NODE_LIB_IMPLS})

NODE_INTFS := \
  node/main/node_identity_file.mli \
  node/main/node_config_file.mli \
  node/main/node_shared_arg.mli \
  node/main/node_run_command.mli \
  node/main/node_config_command.mli \
  node/main/node_identity_command.mli \

NODE_IMPLS := \
  node/main/node_identity_file.ml \
  node/main/node_config_file.ml \
  node/main/node_shared_arg.ml \
  node/main/node_run_command.ml \
  node/main/node_config_command.ml \
  node/main/node_identity_command.ml \
  node_main.ml \

NODE_PACKAGES := \
  $(COMPILER_PACKAGES) \
  calendar \
  cmdliner \
  cohttp.lwt \
  dynlink \
  git \
  ipv6-multicast \
  irmin.unix \
  ocplib-resto.directory \


EMBEDDED_NODE_PROTOCOLS := \
  $(patsubst proto/%/,proto/embedded_proto_%.cmxa, \
  $(filter-out proto/environment/, \
	$(subst TEZOS_PROTOCOL,,$(shell ls proto/*/TEZOS_PROTOCOL))))

NODE_OBJS := \
  ${NODE_IMPLS:.ml=.cmx} ${NODE_IMPLS:.ml=.ml.deps} \
  ${NODE_INTFS:.mli=.cmi} ${NODE_INTFS:.mli=.mli.deps} \
  ${NODE_LIB_IMPLS:.ml=.cmx} ${NODE_LIB_IMPLS:.ml=.ml.deps} \
  ${NODE_LIB_INTFS:.mli=.cmi} ${NODE_LIB_INTFS:.mli=.mli.deps} \
  ${TZNODE}
${NODE_OBJS}: PACKAGES=${NODE_PACKAGES}
${NODE_OBJS}: SOURCE_DIRECTORIES=minutils utils compiler node/db node/net node/updater node/shell node/main
${NODE_OBJS}: TARGET="(node.cmxa)"
${NODE_OBJS}: OPENED_MODULES=Error_monad Hash Utils

node/updater/environment.cmi: node/updater/environment.cmx

node.cmxa: ${NODE_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

 ${NODE_IMPLS:.ml=.cmx}: ${EMBEDDED_CLIENT_PROTOCOLS}
${TZNODE}: minutils.cmxa utils.cmxa compiler.cmxa node.cmxa ${EMBEDDED_NODE_PROTOCOLS} ${NODE_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

partial-clean::
	-rm -f proto/*.cm* proto/*.a

node.top: DEVFLAGS=
node.top: ${MINUTILS_LIB_IMPLS} ${UTILS_LIB_IMPLS} ${COMPILER_LIB_IMPLS} ${FULL_NODE_LIB_IMPLS}
	( $(patsubst %, echo "#require \"%\";;" ; ,${NODE_PACKAGES}) ) > .ocamlinit.node
	( $(patsubst %, echo "#mod_use \"%\";;" ; ,${MINUTILS_LIB_IMPLS} ${UTILS_LIB_IMPLS}) ) >> .ocamlinit.node
	( $(patsubst %, echo "open %;;" ; ,Error_monad Hash Utils) ) >> .ocamlinit.node
	( $(patsubst %, echo "#mod_use \"%\";;" ; ,${COMPILER_LIB_IMPLS} ${FULL_NODE_LIB_IMPLS}) ) >> .ocamlinit.node
	utop -init .ocamlinit.node ${OCAMLFLAGS}

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
  client/client_blocks.mli \
  client/client_network.mli \

CLIENT_LIB_IMPLS := \
  client/client_commands.ml \
  client/client_config.ml \
  client/client_node_rpcs.ml \
  client/client_generic_rpcs.ml \
  client/client_helpers.ml \
  client/client_aliases.ml \
  client/client_keys.ml \
  client/client_protocols.ml \
  client/client_blocks.ml \
  client/client_network.ml \

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

webclient_static.ml: $(shell find  webclient_static/)
	ocp-ocamlres webclient_static -o $@

clean::
	-rm -f ${TZCLIENT} $(TZWEBCLIENT)
	-rm -f webclient_static.ml

## Embedded client protocol modules

.SECONDEXPANSION:

client/embedded/client_%.cmx: \
  $(patsubst %.ml, %.cmx, $(NODE_LIB_IMPLS) $(CLIENT_LIB_IMPLS)) \
  proto/client_embedded_proto_%.cmxa \
  $$(shell find client/embedded/% -name webclient -prune -name \*.ml -or -name \*.mli)
	@$(MAKE) -C client/embedded/$* ../client_$*.cmx

client/embedded/webclient_%.cmx: \
  client/embedded/client_%.cmx \
	minutils.cma \
  $$(shell find client/embedded/%/webclient -name \*.ml -or -name \*.mli 2>/dev/null) \
  $$(shell find client/embedded/%/webclient/static/ 2>/dev/null)
	@$(MAKE) -C client/embedded/$* ../webclient_$*.cmx

partial-clean::
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

.PHONY: clean partial-clean
clean:: partial-clean
partial-clean::
	-find \( -name \*.cm\* -or -name \*.cmp -or -name \*.out -or -name \*~ -or -name \*.o -or -name \*.a \) -delete

## Dependencies

NO_DEPS += \
   compiler/embedded_cmis.ml \
   compiler/embedded_cmis.mli
compiler/embedded_cmis.cmx compiler/embedded_cmis.cmi: OPENED_MODULES=

ifneq ($(MAKECMDGOALS),clean)
include .depend
endif
DEPENDS := $(filter-out $(NO_DEPS), \
  $(MINUTILS_LIB_INTFS) $(MINUTILS_LIB_IMPLS) \
  $(UTILS_LIB_INTFS) $(UTILS_LIB_IMPLS) \
  ${COMPILER_PRECOMPILED_INTFS} \
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

partial-clean::
	-rm -f .depend
	-find \( -name \*.mli.deps -or -name \*.ml.deps \) -delete
	-find \( -name \*.mli.deps.byte -or -name \*.ml.deps.byte \) -delete


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

proto/client_embedded_proto_genesis.cmxa: proto/client_embedded_proto_alpha.cmxa

clean::
	-rm -rf $(patsubst proto/embedded_proto_%.cmxa,proto/%/_tzbuild, \
		   ${EMBEDDED_NODE_PROTOCOLS})
	-rm -f $(patsubst proto/client_embedded_proto_%.cmxa, \
	                proto/register_client_embedded_proto_%.ml, \
			${EMBEDDED_CLIENT_PROTOCOLS})
