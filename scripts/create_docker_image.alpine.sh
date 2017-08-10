#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos/opam}"
image_version="${2:-alpine-${alpine_version}_ocaml-${ocaml_version}}"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf Dockerfile
}
trap cleanup EXIT INT

sed scripts/Dockerfile.alpine.in \
    -e 's|$alpine_version|'"$alpine_version"'|g' \
    -e 's|$ocaml_version|'"$ocaml_version"'|g' > Dockerfile

echo
echo "### Building base image..."
echo

docker build --pull -t "$image_name:$image_version" .

rm Dockerfile
