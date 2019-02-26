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
    rm -rf "$tmp_dir"
    rm -rf Dockerfile
}
trap cleanup EXIT INT

image_name="${1:-tezos_build}"
image_version="${2:-latest}"
base_image="${3-${image_name}_deps:${image_version}}"

mkdir -p "$tmp_dir"/tezos/scripts
cp -a Makefile "$tmp_dir"/tezos
cp -a active_protocol_versions "$tmp_dir"/tezos
cp -a scripts/version.sh "$tmp_dir"/tezos/scripts/
cp -a src "$tmp_dir"/tezos
cp -a vendors "$tmp_dir"/tezos

cat <<EOF > "$tmp_dir"/Dockerfile
FROM $base_image
COPY --chown=tezos:nogroup tezos tezos
RUN opam exec -- make -C tezos all build-test
EOF

echo
echo "### Building tezos..."
echo

docker build -t "$image_name:$image_version" "$tmp_dir"

echo
echo "### Successfully build docker image: $image_name:$image_version"
echo
