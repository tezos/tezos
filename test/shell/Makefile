
SRCDIR=../../src

TESTS := \
  store \
  state \
  context \

include ../Makefile.shared

SOURCE_DIRECTORIES := ${NODE_SOURCE_DIRECTORIES} ../lib

LIB := ${PACKERLIB} ${MINUTILSLIB} ${UTILSLIB} ${COMPILERLIB} ${NODELIB} ${EMBEDDED_NODE_PROTOCOLS} ${TESTLIB}

PACKAGES := \
  ${NODE_PACKAGES} \
  kaputt \

OPENED_MODULES := ${NODE_OPENED_MODULES}

############################################################################
## Store

.PHONY:run-test-store
run-test-store:
	@echo
	./test-store

TEST_STORE_IMPLS := \
  test_store.ml

test-store: ${LIB} ${TEST_STORE_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-store

############################################################################
## State

.PHONY:run-test-state
run-test-state:
	@echo
	./test-state

TEST_STATE_IMPLS := \
  test_state.ml

test-state: ${LIB} ${TEST_STATE_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-state

############################################################################
## Context

.PHONY:run-test-context
run-test-context:
	@echo
	./test-context

TEST_CONTEXT_IMPLS := \
  test_context.ml

test-context: ${LIB} ${TEST_CONTEXT_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-context

