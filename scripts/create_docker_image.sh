#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos}"
image_version="${2:-latest}"
build_image_name="${image_name}_build"
build_deps_image_name="${image_name}_build_deps"

./scripts/create_docker_image.build_deps.sh \
    "$build_deps_image_name" "$image_version"

./scripts/create_docker_image.build.sh \
    "$build_image_name" "$image_version" "$build_deps_image_name"

./scripts/create_docker_image.minimal.sh \
    "$image_name" "$image_version" "$build_image_name"
