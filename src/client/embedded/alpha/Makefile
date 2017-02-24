
PROTO_VERSION := alpha

CLIENT_INTFS := \
  concrete_parser.mli \
  client_proto_rpcs.mli \
  client_proto_args.mli \
  client_proto_contracts.mli \
  client_proto_programs.mli \
  client_proto_context.mli \
  client_proto_nonces.mli \
  client_proto_main.mli

CLIENT_IMPLS := \
  script_located_ir.ml \
  concrete_parser.ml concrete_lexer.ml \
  client_proto_rpcs.ml \
  client_proto_args.ml \
  client_proto_contracts.ml \
  client_proto_programs.ml \
  client_proto_context.ml \
  client_proto_nonces.ml \
  client_proto_main.ml

WEBCLIENT_INTFS := \
  webclient/shared/webclient_proto_services.mli

WEBCLIENT_IMPLS := \
  webclient/webclient_proto_static.ml \
  webclient/shared/webclient_proto_services.ml \
  webclient/webclient_proto_service_directory.ml \
  webclient/webclient_proto_main.ml

SOURCE_DIRECTORIES := webclient/shared webclient/browser webclient

-include baker/Makefile

JS_IMPLS := \
  webclient/shared/webclient_proto_services.ml \
  webclient/browser/webclient_main.ml

JS_INTFS := \
  webclient/shared/webclient_proto_services.mli

JS_DEPS := $(patsubst %,%.deps.byte,${JS_IMPLS} ${JS_INTFS})

DEPENDS += ${JS_DEPS}

# the generated .ml depends on the result of the bytecode compilation
NODEPENDS := webclient/webclient_proto_static.ml.deps

include ../Makefile.shared

${WEBOBJS}: OPENED_MODULES += Tezos_context
${OBJS}: OPENED_MODULES += Tezos_context

predepend: concrete_parser.ml concrete_lexer.ml

webclient/webclient_proto_static.ml: $(shell find webclient/static/*) webclient/static/main.js
	ocp-ocamlres webclient/static -o $@

webclient/webclient_proto_main.cmx: webclient/webclient_proto_static.cmx

.PHONY: clean
clean::
	-rm -f webclient/webclient_proto_static.ml
	-rm -f webclient/*/*.cm* webclient/*/*~ webclient/*/*.o webclient/*/*.a
	-rm -f webclient/*/*.deps webclient/*/*.deps.byte
	-rm -f baker/*.cm* baker/*~ baker/*.o baker/*.a
	-rm -f baker/*.deps baker/*.deps.byte

.INTERMEDIATE: webclient/browser/main.byte

JS_PACKAGES := \
  lwt \
  cstruct \
  ocplib-json-typed.browser \
  ocplib-json-typed.bson \
  ocplib-resto.directory \
  js_of_ocaml.tyxml \
  js_of_ocaml.ppx
${JS_DEPS} ${JS_IMPLS:.ml=.cmo} ${JS_INTFS:.mli=.cmi} webclient/browser/main.byte: \
  PACKAGES=${JS_PACKAGES}
${JS_DEPS} ${JS_IMPLS:.ml=.cmo} ${JS_INTFS:.mli=.cmi} webclient/browser/main.byte: \
  OPENED_MODULES=
${JS_DEPS} ${JS_IMPLS:.ml=.cmo} ${JS_INTFS:.mli=.cmi} webclient/browser/main.byte: \
  SOURCE_DIRECTORIES=../../../minutils webclient/shared webclient/browser

webclient/static/main.js: webclient/browser/main.byte
	@echo JS_OF_OCAML $(notdir $@)
	@js_of_ocaml +weak.js $< -o $@

webclient/browser/main.byte: \
  ../../../minutils.cma \
  ${JS_IMPLS:.ml=.cmo}
	@echo LINK $(notdir $@)
	${OCAMLC} ${OCAMLFLAGS} -o $@ $^ -linkpkg
