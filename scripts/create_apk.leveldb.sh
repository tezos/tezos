#!/bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh

image_name="${1:-tezos/leveldb}"

cat <<EOF > Dockerfile
FROM andyshinn/alpine-abuild:v4

ENV PACKAGER "Tezos <team@tezos.com>"
WORKDIR /home/builder/
RUN abuild-keygen -a -i
COPY scripts/leveldb-$leveldb_version.APKBUILD APKBUILD
RUN abuilder -r
EOF

echo
echo "### Building leveldb..."
echo

docker build -t $image_name:$leveldb_version .
rm Dockerfile

mkdir -p _apk

docker create --name tezos.leveldb.tmp $image_name:$leveldb_version
docker cp -L tezos.leveldb.tmp:/etc/apk/keys _apk/
docker cp -L tezos.leveldb.tmp:/packages _apk
docker rm tezos.leveldb.tmp
