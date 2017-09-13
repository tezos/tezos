
-include Makefile.local
include Makefile.config
include Makefile.files

TZCOMPILER=../tezos-protocol-compiler
TZNODE=../tezos-node
TZCLIENT=../tezos-client
TZATTACKER=../tezos-attacker

all: ${TZCOMPILER} ${TZNODE} ${TZCLIENT} # ${TZATTACKER}

NODEPS :=

############################################################################
## Protocol environment
############################################################################

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
  compiler/sigs/proto_environment.mli compiler/sigs/protocol_sigs.cmi \
  compiler/sigs/camlinternalFormatBasics.cmi
	@echo OCAMLOPT ${TARGET} $@
	@$(OCAMLOPT) -nopervasives -nostdlib -opaque -I tmp -I compiler/sigs -c $<

partial-clean::
	rm -f node/updater/proto_environment.mli
	rm -f compiler/environment_gen

############################################################################
## Protocol environment
############################################################################


compiler/sigs/register.cmi: EXTRA_OCAMLFLAGS = -opaque

COMPILER_PRECOMPILED_OBJS := \
  ${COMPILER_PRECOMPILED_INTFS:.mli=.cmi} \
  ${COMPILER_PRECOMPILED_INTFS:.mli=.mli.deps}

${COMPILER_PRECOMPILED_OBJS}: PACKAGES=${COMPILER_PRECOMPILED_PACKAGES}
${COMPILER_PRECOMPILED_OBJS}: SOURCE_DIRECTORIES=${COMPILER_PRECOMPILED_SOURCE_DIRECTORIES}
${COMPILER_PRECOMPILED_OBJS}: TARGET="(embedded_cmis.cmx)"
${COMPILER_PRECOMPILED_OBJS}: OPENED_MODULES=${COMPILER_PRECOMPILED_OPENED_MODULES}

compiler/sigs/camlinternalFormatBasics.cmi:
	ln -sf $(shell ocamlc -where)/camlinternalFormatBasics.cmi $@

.INTERMEDIATE: compiler/embedded_cmis.ml
compiler/embedded_cmis.cmx: compiler/embedded_cmis.cmi

compiler/embedded_cmis.ml: ${COMPILER_EMBEDDED_CMIS}
	@echo OCAMLRES ${TARGET} $(notdir $@)
	@$(OCAMLRES) -format ocaml -o $@ $^

partial-clean::
	rm -f compiler/embedded_cmis.ml

NO_DEPS += \
  node/updater/protocol_sigs.mli \
  node/updater/proto_environment.mli \
  node/updater/register.mli \
  node/db/persist.mli \
  node/db/context.mli

node/updater/protocol_sigs.cmi: compiler/sigs/protocol_sigs.cmi
	@cp -a compiler/sigs/protocol_sigs.cmi node/updater
node/updater/proto_environment.cmi: compiler/sigs/proto_environment.cmi
	@cp -a compiler/sigs/proto_environment.cmi node/updater
node/updater/register.cmi: compiler/sigs/register.cmi
	@cp -a compiler/sigs/register.cmi node/updater

node/db/persist.cmi: compiler/sigs/persist.cmi
	@cp -a compiler/sigs/persist.cmi node/db
node/db/context.cmi: compiler/sigs/context.cmi
	@cp -a compiler/sigs/context.cmi node/db


############################################################################
## Minimal utils library compatible with js_of_ocaml
############################################################################

MINUTILS_OBJS := \
  ${MINUTILS_LIB_IMPLS:.ml=.cmx} ${MINUTILS_LIB_IMPLS:.ml=.ml.deps} \
  ${MINUTILS_LIB_INTFS:.mli=.cmi} ${MINUTILS_LIB_INTFS:.mli=.mli.deps}
${MINUTILS_OBJS}: PACKAGES=${MINUTILS_PACKAGES}
${MINUTILS_OBJS}: SOURCE_DIRECTORIES=${MINUTILS_SOURCE_DIRECTORIES}
${MINUTILS_OBJS}: TARGET="(minutils.cmxa)"
${MINUTILS_OBJS}: OPENED_MODULES=${MINUTILS_OPENED_MODULES}

minutils.cmxa: ${MINUTILS_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

MINUTILS_OBJS_BYTECODE := \
  ${MINUTILS_LIB_IMPLS:.ml=.cmo} ${MINUTILS_LIB_IMPLS:.ml=.ml.deps.byte}
${MINUTILS_OBJS_BYTECODE}: PACKAGES=${MINUTILS_PACKAGES}
${MINUTILS_OBJS_BYTECODE}: SOURCE_DIRECTORIES=${MINUTILS_SOURCE_DIRECTORIES}
${MINUTILS_OBJS_BYTECODE}: TARGET="(minutils.cma)"
${MINUTILS_OBJS_BYTECODE}: OPENED_MODULES=${MINUTILS_OPENED_MODULES}

minutils.cma: ${MINUTILS_LIB_IMPLS:.ml=.cmo}
	@echo LINK $(notdir $@)
	@${OCAMLC} ${OCAMLFLAGS} -a -o $@ $^

############################################################################
## Utils library
############################################################################

UTILS_OBJS := \
  ${UTILS_LIB_IMPLS:.ml=.cmx} ${UTILS_LIB_IMPLS:.ml=.ml.deps} \
  ${UTILS_LIB_INTFS:.mli=.cmi} ${UTILS_LIB_INTFS:.mli=.mli.deps}
${UTILS_OBJS}: PACKAGES=${UTILS_PACKAGES}
${UTILS_OBJS}: SOURCE_DIRECTORIES=${UTILS_SOURCE_DIRECTORIES}
${UTILS_OBJS}: TARGET="(utils.cmxa)"
${UTILS_OBJS}: OPENED_MODULES=${UTILS_OPENED_MODULES}

utils.cmxa: ${UTILS_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

utils.top: DEVFLAGS=
utils.top:
	( $(patsubst %, echo "#require \"%\";;" ; ,${UTILS_PACKAGES}) ) > .ocamlinit.utils
	( $(patsubst %, echo "#mod_use \"%\";;" ; ,${MINUTILS_LIB_IMPLS} ${UTILS_LIB_IMPLS}) ) >> .ocamlinit.utils
	utop -init .ocamlinit.utils ${UTOPFLAGS}

############################################################################
## Node protocol compiler (also embedded in the main program)
############################################################################

COMPILER_OBJS := \
  ${COMPILER_IMPLS:.ml=.cmx} ${COMPILER_IMPLS:.ml=.ml.deps} \
  ${COMPILER_LIB_IMPLS:.ml=.cmx} ${COMPILER_LIB_IMPLS:.ml=.ml.deps} \
  ${COMPILER_LIB_INTFS:.mli=.cmi} ${COMPILER_LIB_INTFS:.mli=.mli.deps} \
  ${TZCOMPILER}
${COMPILER_OBJS}: PACKAGES=${COMPILER_PACKAGES}
${COMPILER_OBJS}: SOURCE_DIRECTORIES=${COMPILER_SOURCE_DIRECTORIES}
${COMPILER_OBJS}: TARGET="(compiler.cmxa)"
${COMPILER_OBJS}: OPENED_MODULES=${COMPILER_OPENED_MODULES}

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

NODE_OBJS := \
  ${NODE_IMPLS:.ml=.cmx} ${NODE_IMPLS:.ml=.ml.deps} \
  ${NODE_INTFS:.mli=.cmi} ${NODE_INTFS:.mli=.mli.deps} \
  ${NODE_LIB_IMPLS:.ml=.cmx} ${NODE_LIB_IMPLS:.ml=.ml.deps} \
  ${NODE_LIB_INTFS:.mli=.cmi} ${NODE_LIB_INTFS:.mli=.mli.deps} \
  ${TZNODE}
${NODE_OBJS}: PACKAGES=${NODE_PACKAGES}
${NODE_OBJS}: SOURCE_DIRECTORIES=${NODE_SOURCE_DIRECTORIES}
${NODE_OBJS}: TARGET="(node.cmxa)"
${NODE_OBJS}: OPENED_MODULES=${NODE_OPENED_MODULES}

node/updater/environment.cmi: node/updater/environment.cmx

node.cmxa: ${NODE_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

 ${NODE_IMPLS:.ml=.cmx}: ${EMBEDDED_NODE_PROTOCOLS}
${TZNODE}: minutils.cmxa utils.cmxa compiler.cmxa node.cmxa ${EMBEDDED_NODE_PROTOCOLS} ${NODE_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

partial-clean::
	-rm -f proto/*.cm* proto/*.a

node.top: DEVFLAGS=
node.top: ${MINUTILS_LIB_IMPLS} ${UTILS_LIB_IMPLS} ${COMPILER_LIB_IMPLS} ${FULL_NODE_LIB_IMPLS}
	( $(patsubst %, echo "#require \"%\";;" ; ,${NODE_PACKAGES}) ) > .ocamlinit.node
	( $(patsubst %, echo "#mod_use \"%\";;" ; ,${MINUTILS_LIB_IMPLS} ${UTILS_LIB_IMPLS}) ) >> .ocamlinit.node
	( $(patsubst %, echo "open %;;" ; ,${NODE_OPENED_MODULES}) ) >> .ocamlinit.node
	( $(patsubst %, echo "#mod_use \"%\";;" ; ,${COMPILER_LIB_IMPLS} ${FULL_NODE_LIB_IMPLS}) ) >> .ocamlinit.node
	utop -I +compiler-libs ocamlcommon.cma -init .ocamlinit.node ${UTOPFLAGS}
## HACK: the explicit loading of `ocamlcommon.cma` is here to
## circumvent a bug in utop. https://github.com/diml/utop/issues/213

clean::
	rm -f ${TZNODE}

############################################################################
## Client program
############################################################################

CLIENT_OBJS := \
  ${CLIENT_IMPLS:.ml=.cmx} ${CLIENT_IMPLS:.ml=.ml.deps} \
  ${CLIENT_LIB_IMPLS:.ml=.cmx} ${CLIENT_LIB_IMPLS:.ml=.ml.deps} \
  ${CLIENT_LIB_INTFS:.mli=.cmi} ${CLIENT_LIB_INTFS:.mli=.mli.deps} \
  ${TZCLIENT}
${CLIENT_OBJS}: PACKAGES=${CLIENT_PACKAGES}
${CLIENT_OBJS}: SOURCE_DIRECTORIES=${CLIENT_SOURCE_DIRECTORIES}
${CLIENT_OBJS}: OPENED_MODULES=${CLIENT_OPENED_MODULES}

${CLIENT_LIB_IMPLS:.ml=.cmx} ${CLIENT_LIB_IMPLS:.ml=.ml.deps}: TARGET="(client.cmxa)"
${CLIENT_LIB_INTFS:.mli=.cmi} ${CLIENT_LIB_INTFS:.mli=.mli.deps}: TARGET="(client.cmxa)"

client.cmxa: ${CLIENT_LIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^

${EMBEDDED_CLIENT_VERSIONS}: client.cmxa ${EMBEDDED_CLIENT_PROTOCOLS}
${CLIENT_IMPLS:.ml=.cmx}: ${EMBEDDED_CLIENT_VERSIONS}

${TZCLIENT}: minutils.cmxa utils.cmxa compiler.cmxa node.cmxa \
             client.cmxa \
             ${EMBEDDED_CLIENT_PROTOCOLS} \
             ${EMBEDDED_CLIENT_VERSIONS} \
             ${CLIENT_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	-rm -f ${TZCLIENT}

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
	-find . \( -name \*.cm\* -or -name \*.cmp -or -name \*.out -or -name \*~ -or -name \*.o -or -name \*.a \) -delete

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
  $(ATTACKER_LIB_INTFS) $(ATTACKER_LIB_IMPLS) \
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
	-find . \( -name \*.mli.deps -or -name \*.ml.deps \) -delete
	-find . \( -name \*.mli.deps.byte -or -name \*.ml.deps.byte \) -delete


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
  minutils utils compiler node/updater node/db node/net node/shell client \
  $(shell ocamlfind query lwt ocplib-json-typed sodium zarith)

proto/client_embedded_proto_%.cmxa: \
  ${TZCOMPILER} \
  node/updater/environment.cmi \
  node/updater/environment.cmx \
  node/updater/proto_environment.cmi \
  node/updater/proto_environment.cmx \
  proto/%/TEZOS_PROTOCOL \
  $$(wildcard proto/%/*.ml) \
  $$(wildcard proto/%/*.mli)
	@./${TZCOMPILER} -static -client ${DEVFLAGS} \
	   -build-dir client/embedded/$*/_tzbuild \
	   $(addprefix -I , ${CLIENT_PROTO_INCLUDES}) \
	   $@ proto/$*

partial-clean::
	-rm -rf $(patsubst proto/embedded_proto_%.cmxa,proto/%/_tzbuild, \
		   ${EMBEDDED_NODE_PROTOCOLS})
	-rm -f $(patsubst proto/client_embedded_proto_%.cmxa, \
	                proto/register_client_embedded_proto_%.ml, \
			${EMBEDDED_CLIENT_PROTOCOLS})

## Embedded client protocol modules

client/embedded/client_%.cmx: \
  $(patsubst %.ml, %.cmx, $(NODE_LIB_IMPLS) $(CLIENT_LIB_IMPLS)) \
  proto/client_embedded_proto_%.cmxa \
  $$(shell find client/embedded/% \( -name _tzbuild \) -prune -or \( -name \*.ml -print -or -name \*.mli -print \))
	@$(MAKE) -C client/embedded/$* ../client_$*.cmx

partial-clean::
	-for d in $$(ls -d client/embedded/*/) ; do make clean -C $$d ; done
	-rm -f client/embedded/*.cm* client/embedded/*.o
