#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos}"
image_version="${2:-latest}"
build_image_name="${3:-${image_name}_build:${image_version}}"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf Dockerfile bin
}
trap cleanup EXIT INT

# assume $build_image_name has already been created
mkdir -p _docker_build_result
docker create --name tmp1 $build_image_name
docker cp -L tmp1:/home/opam/tezos/_build/install/default/bin/tezos-client _docker_build_result/
docker cp -L tmp1:/home/opam/tezos/_build/install/default/bin/tezos-node _docker_build_result/
docker rm tmp1

# assume tezos/leveldb has already been created
mkdir -p _docker_build_result/leveldb
mkdir -p _docker_build_result/keys
docker create --name tmp1 tezos/leveldb
docker cp -L tmp1:/etc/apk/keys _docker_build_result/
docker cp -L tmp1:/packages _docker_build_result/
docker rm tmp1

echo
echo "### Building minimal docker image..."
echo

sed -e 's|$alpine_version|'"$alpine_version"'|g' \
    scripts/Dockerfile.minimal.in > Dockerfile
docker build -t "$image_name:$image_version" .

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo

rm -Rf _docker_build_result
