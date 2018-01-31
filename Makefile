
DEV ?= --dev

all:
	@jbuilder build ${DEV} \
		src/bin_node/main.exe \
		src/bin_client/main.exe \
		src/bin_client/admin_main.exe \
		src/lib_protocol_compiler/main_native.exe
	@cp _build/default/src/bin_node/main.exe tezos-node
	@cp _build/default/src/bin_client/main.exe tezos-client
	@cp _build/default/src/bin_client/admin_main.exe tezos-admin-client
	@cp _build/default/src/lib_protocol_compiler/main_native.exe tezos-protocol-compiler

tezos-%.pkg:
	@jbuilder build --dev $(patsubst %.opam,%.install, \
				   $(shell find -name tezos-$*.opam))

doc-html: all
	@jbuilder build @doc ${DEV}
	@./tezos-client -protocol ProtoALphaALph man -verbosity 3 -format html | sed "s/$HOME/\$HOME/g" > docs/api/tezos-client.html
	@./tezos-admin-client man -verbosity 3 -format html | sed "s/$HOME/\$HOME/g" > docs/api/tezos-admin-client.html
	@mkdir -p $$(pwd)/docs/_build/api/odoc
	@rm -rf $$(pwd)/docs/_build/api/odoc/*
	@cp -r $$(pwd)/_build/default/_doc/* $$(pwd)/docs/_build/api/odoc/
	@make -C docs

build-test:
	@jbuilder build @buildtest ${DEV}

test:
	@jbuilder runtest ${DEV}
	@./scripts/check_opam_test.sh

test-indent:
	@jbuilder build @runtest_indent ${DEV}

build-deps:
	@./scripts/install_build_deps.sh

docker-image:
	@./scripts/create_docker_image.sh

install:
	@jbuilder build @install
	@jbuilder install

clean:
	@-jbuilder clean
	@-rm -f tezos-node tezos-client tezos-protocol-compiler
	@-make -C docs clean

.PHONY: all test build-deps docker-image clean

