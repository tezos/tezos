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

"$ci_dir"/create_apk.leveldb.sh

cp -a "$build_dir"/leveldb-$leveldb_version-r0.apk \
      "$build_dir"/keys/ \
      "$tmp_dir"

mkdir -p "$tmp_dir"/bin
container=$(docker create $build_image_name)
for bin in tezos-client tezos-node; do
    docker cp -L $container:/home/opam/tezos/$bin "$tmp_dir"/bin
done
cp -a "$script_dir"/docker_entrypoint.sh "$tmp_dir"/bin/tezos
cp -a "$script_dir"/docker_entrypoint.inc.sh "$tmp_dir"/bin/

echo
echo "### Building minimal docker image..."
echo

cat > "$tmp_dir"/Dockerfile <<EOF
FROM alpine:$alpine_version

LABEL distro_style="apk" distro="alpine" distro_long="alpine-$alpine_version" arch="x86_64" operatingsystem="linux"

COPY keys /etc/apk/keys/
COPY leveldb-$leveldb_version-r0.apk .
COPY bin .

RUN adduser -S tezos && \
    adduser tezos abuild && \
    apk add --no-cache sudo bash \
            libssl1.0 libsodium libev gmp git snappy \
            leveldb-$leveldb_version-r0.apk && \
    echo 'tezos ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/tezos && \
    chmod 440 /etc/sudoers.d/tezos && \
    chown root:root /etc/sudoers.d/tezos && \
    sed -i 's/^Defaults.*requiretty//g' /etc/sudoers

USER tezos

COPY . /home/tezos

WORKDIR /home/tezos

RUN sudo chown root:root bin/* && \
    sudo chmod a+rx bin/* && \
    sudo mv bin/* /usr/local/bin && \
    rmdir bin

RUN sudo mkdir -p /var/run/tezos && \
    sudo chown tezos /var/run/tezos

ENV EDITOR=vi

VOLUME /var/run/tezos

ENTRYPOINT [ "/usr/local/bin/tezos" ]
EOF

docker build -t "$image_name:$image_version" "$tmp_dir"

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo
