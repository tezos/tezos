#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

export build_dir=${build_dir:-_docker_build}
tmp_dir=$(mktemp -dt tezos.opam.tezos.XXXXXXXX)

image_name="${1:-tezos}"
image_version="${2:-latest}"
build_image_name="${3:-${image_name}_build:${image_version}}"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf "$tmp_dir"
    if ! [ -z "$container" ]; then docker rm $container; fi
}
trap cleanup EXIT INT

"$ci_dir"/create_apk.hidapi.sh

cp -a "$build_dir"/hidapi-$hidapi_version-r0.apk \
      "$build_dir"/keys/ \
      "$tmp_dir"

mkdir -p "$tmp_dir"/bin
mkdir -p "$tmp_dir"/scripts
container=$(docker create $build_image_name)
for bin in tezos-client tezos-admin-client tezos-node; do
    docker cp -L $container:/home/opam/tezos/$bin "$tmp_dir"/bin
done
cp -a "$script_dir"/docker/entrypoint.sh "$tmp_dir"/bin/
cp -a "$script_dir"/docker/entrypoint.inc.sh "$tmp_dir"/bin/
cp "$script_dir"/alphanet.sh "$tmp_dir"/scripts/
cp "$script_dir"/alphanet_version "$tmp_dir"/scripts/
cp "$src_dir"/src/bin_client/bash-completion.sh "$tmp_dir"/scripts/

echo
echo "### Building minimal docker image..."
echo

cat > "$tmp_dir"/Dockerfile <<EOF
FROM alpine:$alpine_version

LABEL distro_style="apk" distro="alpine" distro_long="alpine-$alpine_version" arch="x86_64" operatingsystem="linux"

COPY keys /etc/apk/keys/
COPY hidapi-$hidapi_version-r0.apk .

RUN apk --no-cache add \
      libev gmp vim leveldb-$leveldb_version-r0.apk hidapi-$hidapi_version-r0.apk && \
    rm hidapi-$hidapi_version-r0.apk

COPY bin/* /usr/local/bin/

COPY scripts/* /usr/local/share/tezos/

RUN adduser -S tezos && \
    mkdir -p /var/run/tezos/node /var/run/tezos/client && \
    chown -R tezos /var/run/tezos

USER tezos

ENV EDITOR=/usr/bin/vi

VOLUME /var/run/tezos/node
VOLUME /var/run/tezos/client

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
EOF

docker build -t "$image_name:$image_version" "$tmp_dir"

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo
