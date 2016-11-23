#! /bin/sh

set -x
set -e

dir="$(mktemp -d)"
src_dir="$(dirname "$(readlink -f "$0")")"

image_name="${1:-tezos_build}"
base_image="${2:-alpine_ocaml-4.03.0}"
image_version="$3"

docker pull ocaml/opam:"$base_image"

cd "$dir"

cp "$src_dir"/install_build_deps.sh "$dir"
cp "$src_dir"/../src/tezos-deps.opam "$dir"

cp "$src_dir"/Dockerfile.build_deps.in "$dir"
sed Dockerfile.build_deps.in -e 's/$base_image/'"$base_image"'/g' > Dockerfile.build_deps

docker build -f Dockerfile.build_deps -t "$image_name:$base_image$image_version" "$dir"
