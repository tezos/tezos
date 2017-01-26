#! /bin/sh

set -e

script_dir="$(dirname "$(readlink -f "$0")")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos_build_deps}"
image_version="${2:-latest}"

sed scripts/Dockerfile.build_deps.in \
    -e 's/$alpine_version/'"$alpine_version"'/g' \
    -e 's/$ocaml_version/'"$ocaml_version"'/g' > Dockerfile

docker build --pull -t "$image_name:$image_version" .

rm Dockerfile
