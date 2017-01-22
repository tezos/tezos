
all:
	${MAKE} -C src all

clean:
	${MAKE} -C src clean
	${MAKE} -C test clean

partial-clean:
	${MAKE} -C src partial-clean
	${MAKE} -C test clean

.PHONY: test
test:
	${MAKE} -C test

build-deps:
	@./scripts/install_build_deps.sh all

docker-image:
	@./scripts/create_docker_image.sh
