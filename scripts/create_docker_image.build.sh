#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos_build}"
image_version="${2:-latest}"
build_deps_image_name="${3-${image_name}_deps:${image_version}}"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf Dockerfile
}
trap cleanup EXIT INT

sed scripts/Dockerfile.build.in \
    -e 's|$base_image|'"$build_deps_image_name"'|g' > Dockerfile

echo
echo "### Building tezos..."
echo

docker build -t "$image_name:$image_version" .

rm Dockerfile

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo
