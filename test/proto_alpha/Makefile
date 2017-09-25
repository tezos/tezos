
SRCDIR=../../src

TESTS := \
  transaction \
  origination \
  endorsement \
  vote \

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

${SRCDIR}/client/embedded/alpha/_tzbuild/%.cmi: ${SRCDIR}/proto/alpha/%.mli
	${MAKE} -C ${SRCDIR} proto/client_embedded_proto_alpha.cmxa
${SRCDIR}/client/embedded/alpha/_tzbuild/%.cmx: ${SRCDIR}/proto/alpha/%.ml
	${MAKE} -C ${SRCDIR} proto/client_embedded_proto_alpha.cmxa
${SRCDIR}/client/embedded/alpha/%.cmi: ${SRCDIR}/client/embedded/alpha/%.mli
	${MAKE} -C ${SRCDIR} client/embedded/client_alpha.cmx
${SRCDIR}/client/embedded/alpha/%.cmx: ${SRCDIR}/client/embedded/alpha/%.ml
	${MAKE} -C ${SRCDIR} client/embedded/client_alpha.cmx

############################################################################
## Transactions

.PHONY:run-test-transaction
run-test-transaction:
	@echo
	./test-transaction

TEST_TRANSACTION_IMPLS := \
  proto_alpha_helpers.ml \
  test_transaction.ml

test-transaction: ${LIB} ${TEST_TRANSACTION_IMPLS:.ml=.cmx}
	@echo COMPILE $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-transaction

############################################################################
## Origination

.PHONY:run-test-origination
run-test-origination:
	@echo
	./test-origination

TEST_ORIGINATION_IMPLS := \
  proto_alpha_helpers.ml \
  test_origination.ml

test-origination: ${LIB} ${TEST_ORIGINATION_IMPLS:.ml=.cmx}
	@echo COMPILE $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-origination

############################################################################
## Endorsement

.PHONY:run-test-endorsement
run-test-endorsement:
	@echo
	./test-endorsement

TEST_ENDORSEMENT_IMPLS := \
  proto_alpha_helpers.ml \
  test_endorsement.ml

test-endorsement: ${LIB} ${TEST_ENDORSEMENT_IMPLS:.ml=.cmx}
	@echo COMPILE $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-endorsement

############################################################################
## Vote

.PHONY:run-test-vote
run-test-vote:
	@echo
	./test-vote

TEST_VOTE_IMPLS := \
  proto_alpha_helpers.ml \
  test_vote.ml

test-vote: ${LIB} ${TEST_VOTE_IMPLS:.ml=.cmx}
	@echo COMPILE $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-vote

############################################################################
## Michelson Parser

.PHONY: run-test-michelson-parser
run-test-michelson-parser:
	@echo
	./test-michelson-parser

TEST_MICHELSON_PARSER_IMPLS := \
  proto_alpha_helpers.ml \
  test_michelson_parser.ml

test-michelson-parser: ${LIB} ${TEST_MICHELSON_PARSER_IMPLS:.ml=.cmx}
	@echo COMPILE $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-michelson-parser
