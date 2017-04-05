
SRCDIR=../../src

TESTS := \
  transaction \
  origination \
  endorsement \

include ../Makefile.shared

SOURCE_DIRECTORIES := \
  ${CLIENT_SOURCE_DIRECTORIES} \
  ${SRCDIR}/proto \
  ${SRCDIR}/client/embedded \
  ${SRCDIR}/client/embedded/alpha \
  ${SRCDIR}/client/embedded/alpha/baker \
  ../lib

LIB := \
  ${MINUTILSLIB} ${UTILSLIB} ${COMPILERLIB} \
  ${NODELIB} ${CLIENTLIB} ${EMBEDDED_CLIENT_PROTOCOLS} ${TESTLIB}

PACKAGES := \
  ${CLIENT_PACKAGES} \
  kaputt \

OPENED_MODULES := \
  ${CLIENT_OPENED_MODULES} \
  Environment Client_embedded_proto_alpha Tezos_context

############################################################################
## Transactions

.PHONY:run-test-transaction
run-test-transaction:
	@echo
	./test-transaction

TEST_CONNECTION_IMPLS := \
  proto_alpha_helpers.mli \
  proto_alpha_helpers.ml \
  test_transaction.ml

test-transaction: ${LIB} ${TEST_CONNECTION_IMPLS:.ml=.cmx}
	@echo COMPILE $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-transaction

