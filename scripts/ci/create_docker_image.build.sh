#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

tmp_dir=$(mktemp -dt tezos.opam.tezos.XXXXXXXX)

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf Dockerfile
}
trap cleanup EXIT INT

image_name="${1:-tezos_build}"
image_version="${2:-latest}"
base_image_name="${3-${image_name}_deps:${image_version}}"

cat <<EOF > "$src_dir"/Dockerfile
FROM $base_image_name
COPY Makefile tezos/
COPY src tezos/src/
COPY vendors tezos/vendors/
RUN sudo chown -R opam tezos && \
    opam exec -- make -C tezos all build-test
EOF

echo
echo "### Building tezos..."
echo

docker build -t "$image_name:$image_version" "$src_dir"

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo
