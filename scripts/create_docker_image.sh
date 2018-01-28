#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

export build_dir=$(mktemp -dt tezos.docker.XXXXXXXX)

image_name="${1:-tezos}"
image_version="${2:-latest}"
opam_image_name="${image_name}_opam"
build_image_name="${image_name}_build"
build_deps_image_name="${image_name}_build_deps"

"$script_dir"/ci/create_docker_image.opam.sh \
    "$opam_image_name" "$image_version"

"$script_dir"/ci/create_docker_image.build_deps.sh \
    "$build_deps_image_name" "$image_version" "$opam_image_name:$image_version"

"$script_dir"/ci/create_docker_image.build.sh \
    "$build_image_name" "$image_version" "$build_deps_image_name"

"$script_dir"/ci/create_docker_image.minimal.sh \
    "$image_name" "$image_version" "$build_image_name"
