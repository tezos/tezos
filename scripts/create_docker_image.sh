#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

image_name="${1:-tezos}"
image_version="${2:-latest}"
build_deps_image_name=${3:-registry.gitlab.com/tezos/opam-repository}
build_deps_image_version=${4:-$opam_repository_tag}

build_image_name="${image_name}_build"

"$script_dir"/ci/create_docker_image.build.sh \
             "$build_image_name" "$image_version" \
             "$build_deps_image_name:$build_deps_image_version"

"$script_dir"/ci/create_docker_image.minimal.sh \
             "$image_name" "$image_version" \
             "$build_image_name:$image_version" \
             "$build_deps_image_name:minimal--$build_deps_image_version"
