
DEV ?= --dev

all:
	@jbuilder build ${DEV} \
		bin_node/main.exe \
		bin_client/main.exe \
		lib_protocol_compiler/main.exe
	@cp _build/default/bin_node/main.exe tezos-node
	@cp _build/default/bin_client/main.exe tezos-client
	@cp _build/default/lib_protocol_compiler/main.exe tezos-protocol-compiler

doc-html:
	@jbuilder build @doc ${DEV}

build-test:
	@jbuilder build @buildtest ${DEV}

test:
	@jbuilder runtest ${DEV}

build-deps:
	@./scripts/install_build_deps.sh

docker-image:
	@./scripts/create_docker_image.sh

clean:
	@-jbuilder clean
	@-rm -f tezos-node tezos-client tezos-protocol-compiler

.PHONY: all test build-deps docker-image clean
