#!/bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

build_dir=${build_dir:-_docker_build}

if [ -f "$build_dir"/hidapi-$hidapi_version-r0.apk ] && \
   [ -f "$build_dir"/hidapi-dev-$hidapi_version-r0.apk ] && \
   [ -d "$build_dir"/keys/ ] ; then
    exit 0
fi

tmp_image="hidapi.apk-builder"
tmp_dir=$(mktemp -dt tezos.hidapi.XXXXXXXX)
cleanup () {
    set +e
    rm -rf "$tmp_dir"
    if ! [ -z "$container" ]; then docker rm $container; fi
    # docker rmi $tmp_image || true
}
trap cleanup EXIT INT

cp -a "$script_dir"/ci/hidapi-$hidapi_version.APKBUILD "$tmp_dir"/APKBUILD

cat <<EOF > "$tmp_dir/Dockerfile"
FROM andyshinn/alpine-abuild:v4
ENV PACKAGER "Tezos <ci@tezos.com>"
WORKDIR /home/builder/
RUN abuild-keygen -a -i
COPY APKBUILD .
RUN abuilder -r
EOF

echo
echo "### Building hidapi..."
echo

docker build -t $tmp_image "$tmp_dir"

mkdir -p "$build_dir"

container=$(docker create $tmp_image)
docker cp -L $container:/etc/apk/keys "$build_dir"
docker cp -L $container:/packages/home/x86_64/hidapi-$hidapi_version-r0.apk \
             "$build_dir"
docker cp -L $container:/packages/home/x86_64/hidapi-dev-$hidapi_version-r0.apk \
             "$build_dir"
