#!/bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

build_dir=${build_dir:-_docker_build}

if [ -f "$build_dir"/leveldb-$leveldb_version-r0.apk ] && \
   [ -f "$build_dir"/leveldb-dev-$leveldb_version-r0.apk ] && \
   [ -d "$build_dir"/keys/ ] ; then
    exit 0
fi

tmp_image="leveldb.apk-builder"
tmp_dir=$(mktemp -dt tezos.leveldb.XXXXXXXX)
cleanup () {
    set +e
    rm -rf "$tmp_dir"
    if ! [ -z "$container" ]; then docker rm $container; fi
    # docker rmi $tmp_image || true
}
trap cleanup EXIT INT

cp -a "$script_dir"/ci/leveldb-$leveldb_version.APKBUILD "$tmp_dir"/APKBUILD

cat <<EOF > "$tmp_dir/Dockerfile"
FROM andyshinn/alpine-abuild:v4
ENV PACKAGER "Tezos <ci@tezos.com>"
WORKDIR /home/builder/
RUN abuild-keygen -a -i
COPY APKBUILD .
RUN abuilder -r
EOF

echo
echo "### Building leveldb..."
echo

docker build -t $tmp_image "$tmp_dir"

mkdir -p "$build_dir"

container=$(docker create $tmp_image)
docker cp -L $container:/etc/apk/keys "$build_dir"
docker cp -L $container:/packages/home/x86_64/leveldb-$leveldb_version-r0.apk \
             "$build_dir"
docker cp -L $container:/packages/home/x86_64/leveldb-dev-$leveldb_version-r0.apk \
             "$build_dir"
