
SRCDIR=../../src

TESTS := \
  p2p-connection \
  p2p-connection-pool \
  p2p-io-scheduler \

include ../Makefile.shared

SOURCE_DIRECTORIES := ${NODE_SOURCE_DIRECTORIES} ../lib

LIB := ${MINUTILSLIB} ${UTILSLIB} ${COMPILERLIB} ${NODELIB} ${TESTLIB}

PACKAGES := \
  ${NODE_PACKAGES} \
  kaputt \

OPENED_MODULES := ${NODE_OPENED_MODULES}

############################################################################
## P2p_connection

.PHONY:run-test-p2p-connection
run-test-p2p-connection:
	@echo
	./test-p2p-connection -v

TEST_CONNECTION_IMPLS := \
  test_p2p_connection.ml

test-p2p-connection: ${LIB} ${TEST_CONNECTION_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-p2p-connection

############################################################################
## P2p_connection_pool

.PHONY:run-test-p2p-connection-pool
run-test-p2p-connection-pool:
	@echo
	./test-p2p-connection-pool --clients 10 --repeat 5 -v

TEST_CONNECTION_POOL_IMPLS := \
  test_p2p_connection_pool.ml

test-p2p-connection-pool: ${LIB} ${TEST_CONNECTION_POOL_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-p2p-connection-pool

############################################################################
## P2p_io_scheduler

.PHONY:run-test-p2p-io-scheduler
run-test-p2p-io-scheduler:
	@echo
	./test-p2p-io-scheduler \
	   --delay 5 --clients 8 \
	   --max-upload-speed $$((1 << 18)) \
	   --max-download-speed $$((1 << 20))

TEST_IO_SCHEDULER_IMPLS := \
  test_p2p_io_scheduler.ml

test-p2p-io-scheduler: ${LIB} ${TEST_IO_SCHEDULER_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} -linkall -linkpkg ${OCAMLFLAGS} -o $@ $^

clean::
	rm -f test-p2p-io-scheduler

