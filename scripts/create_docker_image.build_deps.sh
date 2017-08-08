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
dependencies="scripts/install_build_deps.sh src/tezos-deps.opam Dockerfile"
dependencies_sha1=$(docker inspect --format="{{ .RootFS.Layers }}" --type=image $base_image | sha1sum - $dependencies | sha1sum | tr -d ' -')
for cached_image in "$@"; do
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
done

echo
echo "### Building tezos dependencies..."
echo

docker build -t "$image_name:$image_version" .

rm Dockerfile

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo

for cached_image in "$@"; do
    echo
    echo "### Saving socker image ($cached_image)..."
    echo
    docker tag "$image_name:$image_version" \
               "$cached_image:$dependencies_sha1"
    docker push "$cached_image:$dependencies_sha1"
done
