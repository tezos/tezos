
all:
	@jbuilder build tezos.install --dev
	@cp _build/default/src/node_main.exe tezos-node
	@cp _build/default/src/client_main.exe tezos-client
	@cp _build/default/src/compiler_main.exe tezos-protocol-compiler

doc-html:
	@jbuilder build @doc --dev

build-test:
	@jbuilder build @buildtest --dev

test:
	@jbuilder runtest

build-deps:
	@./scripts/install_build_deps.sh

docker-image:
	@./scripts/create_docker_image.sh

clean:
	@-jbuilder clean
	@-rm -f tezos-node tezos-client tezos-protocol-compiler

.PHONY: all test build-deps docker-image clean
