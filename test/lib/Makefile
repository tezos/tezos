
SRCDIR=../../src

build: testlib.cmxa

TESTS :=
include ../Makefile.shared

SOURCE_DIRECTORIES := ${NODE_SOURCE_DIRECTORIES} ../lib

PACKAGES := \
  ${NODE_PACKAGES} \
  kaputt \

OPENED_MODULES := ${NODE_OPENED_MODULES}

TESTLIB_INTFS := \
  process.mli \
  test.mli \
  assert.mli \
  node_helpers.mli \

TESTLIB_IMPLS := \
  process.ml \
  test.ml \
  assert.ml \
  node_helpers.ml \

${TESTLIB_IMPLS:.ml=.cmx} ${TESTLIB_INTFS:.mli=.cmi}: TARGET="(testlib.cmxa)"
testlib.cmxa: ${TESTLIB_IMPLS:.ml=.cmx}
	@echo LINK $(notdir $@)
	@${OCAMLOPT} ${OCAMLFLAGS} -a -o $@ $^
