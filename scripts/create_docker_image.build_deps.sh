#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos_build_deps}"
image_version="${2:-latest}"
cached_image="${3:-}"

base_image="tezos/opam:alpine-${alpine_version}_ocaml-${ocaml_version}"
if ! docker pull "$base_image" ; then
    ./scripts/create_docker_image.alpine.sh
fi

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
dependencies="scripts/install_build_deps.sh src/tezos-deps.opam Dockerfile"
dependencies_sha1=$(docker inspect --format="{{ .RootFS.Layers }}" --type=image $base_image | sha1sum - $dependencies | sha1sum | tr -d ' -')
if [ ! -z "$cached_image" ]; then
    echo
    echo "### Looking for prebuilt dependencies ($cached_image)..."
    if docker pull "$cached_image:$dependencies_sha1"; then
        echo
        echo "### Found $cached_image:$dependencies_sha1"
        echo
        docker tag "$cached_image:$dependencies_sha1" \
                   "$image_name:$image_version"
        exit 0
    fi
    echo "### Missing..."
    echo
fi

echo
echo "### Building tezos dependencies..."
echo

docker build -t "$image_name:$image_version" .

rm Dockerfile

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo

if [ ! -z "$cached_image" ]; then
    echo
    echo "### Saving docker image ($cached_image)..."
    echo
    docker tag "$image_name:$image_version" \
               "$cached_image:$dependencies_sha1"
    docker push "$cached_image:$dependencies_sha1"
fi
