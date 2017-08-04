#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos_build_deps}"
image_version="${2:-latest}"
shift 2

base_image="ocaml/opam:alpine-${alpine_version}_ocaml-${ocaml_version}"
docker pull "$base_image"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf Dockerfile
}
trap cleanup EXIT INT

sed scripts/Dockerfile.build_deps.in \
    -e 's|$base_image|'"$base_image"'|g' \
    -e 's|$ocaml_version|'"$ocaml_version"'|g' > Dockerfile

## Lookup for for prebuilt dependencies...
base_layers=$(docker inspect --format="{{ .RootFS.Layers }}" --type=image $base_image | tr -d '[]')
same() {
    docker run --rm "$1" cat /home/opam/$2 | diff -wq $2 -
}
for cached_image in "$@"; do
    if ! docker pull $cached_image; then continue; fi
    cached_base_layers=$(docker inspect --format="{{ .RootFS.Layers }}" --type=image $cached_image | tr -d '[]')
    if [ "${cached_base_layers##$base_layers}" = "$cached_base_layers" ]; then continue; fi
    if ! same "$cached_image" scripts/install_build_deps.sh ; then continue ; fi
    if ! same "$cached_image" src/tezos-deps.opam ; then continue ; fi
    if ! same "$cached_image" Dockerfile ; then continue ; fi
    docker tag "$cached_image" "$image_name:$image_version"
    exit 0
done

echo
echo "### Building tezos dependencies..."
echo

docker build -t "$image_name:$image_version" .

rm Dockerfile

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo
