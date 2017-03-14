#! /bin/sh

set -e

script_dir="$(dirname "$(readlink -f "$0")")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh
image_name="${1:-tezos}"
image_version="${2:-latest}"
build_deps_image_name="$image_name"_build_deps

echo Building dependencies...
./scripts/create_build_deps_docker_image.sh \
    "$build_deps_image_name" "$image_version"

cleanup () {
    set +e
    echo Cleaning up...
    [ -z "$tmp_container" ] || docker rm "$tmp_container"
    [ -z "$tmp_image" ] || docker rmi "$tmp_image"
    rm -rf Dockerfile bin
}
trap cleanup EXIT INT

sed scripts/Dockerfile.build_bin.in \
    -e 's|$base_name|'"$build_deps_image_name"'|g' \
    -e 's|$base_version|'"$image_version"'|g'  > Dockerfile

echo Building tezos...
tmp_image="$(docker build -q .)"
tmp_container="$(docker run -dit "$tmp_image" true)"

docker cp "$tmp_container":/home/opam/bin/ bin

echo Building minimal docker image...
sed scripts/Dockerfile.binaries.in \
    -e 's|$alpine_version|'"$alpine_version"'|g' > Dockerfile
docker build -q -t "$image_name:$image_version" .
