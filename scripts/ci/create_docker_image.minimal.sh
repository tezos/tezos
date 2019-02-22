#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

tmp_dir=$(mktemp -dt tezos.opam.tezos.XXXXXXXX)

image_name="${1:-tezos}"
image_version="${2:-latest}"
build_image="${3:-registry.gitlab.com/tezos/opam-repository:${opam_repository_tag}}"
base_image="${4-registry.gitlab.com/tezos/opam-repository:minimal--${opam_repository_tag}}"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf "$tmp_dir"
    if ! [ -z "$container" ]; then docker rm $container; fi
}
trap cleanup EXIT INT

mkdir -p "$tmp_dir"/bin
mkdir -p "$tmp_dir"/scripts
container=$(docker create $build_image)
versioned_daemons="$(sed "s/^\(.*\)$/tezos-baker-\1 tezos-endorser-\1 tezos-accuser-\1/g" "active_protocol_versions")"
for bin in tezos-client tezos-admin-client tezos-node $versioned_daemons tezos-signer; do
    docker cp -L $container:/home/tezos/tezos/$bin "$tmp_dir"/bin
done
cp -a "$script_dir"/docker/entrypoint.sh "$tmp_dir"/bin/
cp -a "$script_dir"/docker/entrypoint.inc.sh "$tmp_dir"/bin/
cp "$script_dir"/alphanet.sh "$tmp_dir"/scripts/
cp "$script_dir"/alphanet_version "$tmp_dir"/scripts/
cp "$src_dir"/src/bin_client/bash-completion.sh "$tmp_dir"/scripts/
cp "$src_dir"/active_protocol_versions "$tmp_dir"/scripts/

echo
echo "### Building minimal docker image..."
echo

cat > "$tmp_dir"/Dockerfile <<EOF
FROM $base_image

RUN sudo apk --no-cache add vim
ENV EDITOR=/usr/bin/vi

RUN sudo mkdir -p /var/run/tezos/node /var/run/tezos/client && \
    sudo chown -R tezos /var/run/tezos

COPY bin/* /usr/local/bin/

COPY scripts/* /usr/local/share/tezos/

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
EOF

docker build -t "$image_name:$image_version" "$tmp_dir"

echo
echo "### Successfully build docker image: $image_name:$image_version"
echo
