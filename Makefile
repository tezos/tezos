
DEV ?= --dev
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
	@jbuilder build ${DEV} \
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
	@jbuilder build ${DEV} \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name \*.opam -print))

$(addsuffix .pkg,${PACKAGES}): %.pkg:
	@jbuilder build ${DEV} \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name $*.opam -print))

$(addsuffix .test,${PACKAGES}): %.test:
	@jbuilder build ${DEV} \
	    @$(patsubst %/$*.opam,%,$(shell find src vendors -name $*.opam))/runtest

doc-html: all
	@jbuilder build @doc ${DEV}
	@./tezos-client -protocol PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY man -verbosity 3 -format html | sed "s/$HOME/\$HOME/g" > docs/api/tezos-client.html
	@./tezos-admin-client man -verbosity 3 -format html | sed "s/$HOME/\$HOME/g" > docs/api/tezos-admin-client.html
	@mkdir -p $$(pwd)/docs/_build/api/odoc
	@rm -rf $$(pwd)/docs/_build/api/odoc/*
	@cp -r $$(pwd)/_build/default/_doc/* $$(pwd)/docs/_build/api/odoc/
	@${MAKE} -C docs

build-test:
	@jbuilder build @buildtest ${DEV}

test:
	@jbuilder runtest ${DEV}
	@./scripts/check_opam_test.sh

test-indent:
	@jbuilder build @runtest_indent ${DEV}

fix-indent:
	@src/lib_stdlib/test-ocp-indent.sh fix

build-deps:
	@./scripts/install_build_deps.sh

docker-image:
	@./scripts/create_docker_image.sh

install:
	@jbuilder build @install
	@jbuilder install

uninstall:
	@jbuilder uninstall

clean:
	@-jbuilder clean
	@-rm -f \
		tezos-node \
		tezos-client \
		tezos-signer \
		tezos-admin-client \
		tezos-protocol-compiler \
	  $(foreach p, $(active_protocol_versions), tezos-baker-$(p) tezos-endorser-$(p) tezos-accuser-$(p))
	@-${MAKE} -C docs clean

.PHONY: all test build-deps docker-image clean
