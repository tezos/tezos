#!/bin/sh

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
    ./scripts/create_docker_image.alpine.opam2.sh
fi

cleanup () {
    set +e
    echo Cleaning up...
    rm -f Dockerfile opams.tar.gz scripts.tar.gz
}
trap cleanup EXIT INT

dependencies="scripts/install_build_deps.sh scripts/version.sh scripts/opam-pin.sh scripts/opam-unpin.sh scripts/opam-remove.sh"
tar czvf scripts.tar.gz $dependencies

opams=$(find -name \*.opam -type f)
tar czvf opams.tar.gz $opams

cat <<EOF >Dockerfile
FROM $base_image

# these two archives are created in the file
# scripts/create_docker_image.build_deps.sh and removed
# automatically after
ADD opams.tar.gz tezos/
ADD scripts.tar.gz tezos/

USER opam

RUN opam config exec -- ./tezos/scripts/install_build_deps.sh
ENV OPAMYES=yes
RUN opam config exec -- opam install ocp-indent && \
  rm -fr ~/.opam/log/ && \
  rm -fr "\$(opam config exec -- ocamlfind query stdlib)"/topdirs.cmi

EOF

## Lookup for for prebuilt dependencies...
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
echo "### Building tezos dependencies... $image_name:$image_version"
echo

docker build --pull -t "$image_name:$image_version" .

rm -f Dockerfile opams.tar.gz scripts.tar.gz

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
