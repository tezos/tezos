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

docker run -dit --rm  --volume $(pwd)/bin:/home/opam/bin "$build_image_name" \
       /bin/sh -c "sudo cp -L /home/opam/tezos/_build/install/default/bin/* /home/opam/bin"

ls bin

echo
echo "### Building minimal docker image..."
echo

sed -e 's|$alpine_version|'"$alpine_version"'|g' \
    scripts/Dockerfile.minimal.in > Dockerfile
docker build -t "$image_name:$image_version" .

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo

rm -r bin
