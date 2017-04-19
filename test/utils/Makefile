
SRCDIR=../../src

TESTS := \
  merkle \
  data-encoding \
  stream-data-encoding \
  # lwt-pipe

include ../Makefile.shared

SOURCE_DIRECTORIES := ${COMPILER_SOURCE_DIRECTORIES} ../lib

LIB := ${MINUTILSLIB} ${UTILSLIB} ${TESTLIB}

PACKAGES := \
  ${UTILS_PACKAGES} \
  kaputt \

OPENED_MODULES := ${NODE_OPENED_MODULES}

############################################################################
## Lwt_pipe

.PHONY:run-test-lwt-pipe
run-test-lwt-pipe:
	@echo
	./test-lwt-pipe

TEST_PIPE_IMPLS := \
  test_lwt_pipe.ml

test-lwt-pipe: ${LIB} ${TEST_PIPE_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-lwt-pipe

############################################################################
## Merkle

.PHONY:run-test-merkle
run-test-merkle:
	@echo
	./test-merkle

TEST_MERKLE_IMPLS := \
  test_merkle.ml

test-merkle: ${LIB} ${TEST_MERKLE_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-merkle

############################################################################
## Data_encoding

.PHONY:run-test-data-encoding
run-test-data-encoding:
	@echo
	./test-data-encoding

TEST_DATA_ENCODING_IMPLS := \
  test_data_encoding.ml

test-data-encoding: ${LIB} ${TEST_DATA_ENCODING_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-data-encoding

############################################################################
## Streamed data_encoding

.PHONY:run-test-stream-data-encoding
run-test-stream-data-encoding:
	@echo
	./test-stream-data-encoding

TEST_DATA_ENCODING_IMPLS := \
  test_data_encoding.ml

test-stream-data-encoding: ${LIB} ${TEST_DATA_ENCODING_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-stream-data-encoding
