#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

if [ "$1" = "--build-opam" ] ; then
    build_opam=yes
    shift 1
fi
image_name="${1:-tezos}"
image_version="${2:-latest}"
build_dir="${3:-$src_dir/_docker_build}"
opam_image_name="${image_name}_opam"
build_image_name="${image_name}_build"
build_deps_image_name="${image_name}_build_deps"

if [ -n "$build_opam" ] ; then

    export build_dir

    "$script_dir"/ci/create_docker_image.opam.sh \
                 "$opam_image_name" "$image_version"

else

    docker pull registry.gitlab.com/tezos/tezos/opam:latest
    docker tag registry.gitlab.com/tezos/tezos/opam:latest \
           "$opam_image_name:$image_version"

fi

"$script_dir"/ci/create_docker_image.build_deps.sh \
             "$build_deps_image_name" "$image_version" \
             "$opam_image_name:$image_version"


"$script_dir"/ci/create_docker_image.build.sh \
             "$build_image_name" "$image_version" \
             "$build_deps_image_name"

"$script_dir"/ci/create_docker_image.minimal.sh \
             "$image_name" "$image_version" "$build_image_name"
