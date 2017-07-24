
DIR := \
  utils \
  shell \
  p2p \
  proto_alpha

all: build run

.PHONY: build run test
build:
	${MAKE} -C lib build
	${MAKE} ${addprefix build-,${DIR}}
run:
	${MAKE} ${addprefix run-,${DIR}}
	${MAKE} run-basic.sh
	${MAKE} run-contracts.sh
clean:
	${MAKE} -C lib clean
	${MAKE} ${addprefix clean-,${DIR}}

${addprefix build-,${DIR}}: build-%:
	${MAKE} -C $* build
${addprefix run-,${DIR}}: run-%:
	${MAKE} -C $* run
${addprefix clean-,${DIR}}: clean-%:
	${MAKE} -C $* clean

run-basic.sh:
	./test-basic.sh

run-contracts.sh:
	./test-contracts.sh
