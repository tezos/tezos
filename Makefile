
PACKAGES:=$(patsubst %.opam,%,$(notdir $(shell find src vendors -name \*.opam -print)))

active_protocol_versions := $(shell cat active_protocol_versions)
active_protocol_directories := $(shell tr -- - _ < active_protocol_versions)

current_opam_version := $(shell opam --version)
include scripts/version.sh

ifneq (${current_opam_version},${opam_version})
$(error Unexpected opam version (found: ${current_opam_version}, expected: ${opam_version}))
endif

current_ocaml_version := $(shell opam exec -- ocamlc -version)

all:
ifneq (${current_ocaml_version},${ocaml_version})
	$(error Unexpected ocaml version (found: ${current_ocaml_version}, expected: ${ocaml_version}))
endif
	@dune build \
		src/bin_node/main.exe \
		src/bin_client/main_client.exe \
		src/bin_client/main_admin.exe \
		src/bin_signer/main_signer.exe \
		src/lib_protocol_compiler/main_native.exe \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/bin_baker/main_baker_$(p).exe) \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/bin_endorser/main_endorser_$(p).exe) \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/bin_accuser/main_accuser_$(p).exe)
	@cp _build/default/src/bin_node/main.exe tezos-node
	@cp _build/default/src/bin_client/main_client.exe tezos-client
	@cp _build/default/src/bin_client/main_admin.exe tezos-admin-client
	@cp _build/default/src/bin_signer/main_signer.exe tezos-signer
	@cp _build/default/src/lib_protocol_compiler/main_native.exe tezos-protocol-compiler
	@for p in $(active_protocol_directories) ; do \
	   cp _build/default/src/proto_$$p/bin_baker/main_baker_$$p.exe tezos-baker-`echo $$p | tr -- _ -` ; \
	   cp _build/default/src/proto_$$p/bin_endorser/main_endorser_$$p.exe tezos-endorser-`echo $$p | tr -- _ -` ; \
	   cp _build/default/src/proto_$$p/bin_accuser/main_accuser_$$p.exe tezos-accuser-`echo $$p | tr -- _ -` ; \
	 done

all.pkg:
	@dune build \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name \*.opam -print))

$(addsuffix .pkg,${PACKAGES}): %.pkg:
	@dune build \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name $*.opam -print))

$(addsuffix .test,${PACKAGES}): %.test:
	@dune build \
	    @$(patsubst %/$*.opam,%,$(shell find src vendors -name $*.opam))/runtest

doc-html: all
	@dune build @doc
	@./tezos-client -protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > docs/api/tezos-client.html
	@./tezos-admin-client man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > docs/api/tezos-admin-client.html
	@./tezos-signer man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > docs/api/tezos-signer.html
	@./tezos-baker-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > docs/api/tezos-baker-alpha.html
	@./tezos-endorser-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > docs/api/tezos-endorser-alpha.html
	@./tezos-accuser-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > docs/api/tezos-accuser-alpha.html
	@mkdir -p $$(pwd)/docs/_build/api/odoc
	@rm -rf $$(pwd)/docs/_build/api/odoc/*
	@cp -r $$(pwd)/_build/default/_doc/* $$(pwd)/docs/_build/api/odoc/
	@${MAKE} -C docs html

doc-linkcheck:
	@${MAKE} -C docs linkcheck

build-test:
	@dune build @buildtest

test:
	@dune runtest
	@./scripts/check_opam_test.sh

test-indent:
	@dune build @runtest_indent

fix-indent:
	@src/lib_stdlib/test-ocp-indent.sh fix

build-deps:
	@./scripts/install_build_deps.sh

docker-image:
	@./scripts/create_docker_image.sh

install:
	@dune build @install
	@dune install

uninstall:
	@dune uninstall

clean:
	@-dune clean
	@-rm -f \
		tezos-node \
		tezos-client \
		tezos-signer \
		tezos-admin-client \
		tezos-protocol-compiler \
	  $(foreach p, $(active_protocol_versions), tezos-baker-$(p) tezos-endorser-$(p) tezos-accuser-$(p))
	@-${MAKE} -C docs clean
	@-rm -f docs/api/tezos-{baker,endorser,accuser}-alpha.html docs/api/tezos-{admin-,}client.html docs/api/tezos-signer.html

.PHONY: all test build-deps docker-image clean
