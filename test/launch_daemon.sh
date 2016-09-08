#! /bin/sh

set -e

DIR=$(dirname "$0")
cd "${DIR}"

CLIENT_DIR=$(mktemp -d /tmp/tezos_client.XXXXXXXXXX)

cleanup() {
    rm -fr ${CLIENT_DIR}
}
trap cleanup EXIT QUIT INT

# export LWT_LOG="client.endorsement -> info; client.mining -> info"

CLIENT="../tezos-client -base-dir ${CLIENT_DIR}"
${CLIENT} bootstrap
${CLIENT} launch daemon $@
