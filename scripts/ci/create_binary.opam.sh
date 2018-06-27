#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

build_dir=${build_dir:-_docker_build}

if [ -f "$build_dir"/opam-$opam_tag ] && \
   [ -f "$build_dir"/opam-installer-$opam_tag ] ; then
    exit 0
fi

tmp_image="opam.bin-builder"
tmp_dir=$(mktemp -dt tezos.opam.XXXXXXXX)

cleanup () {
    set +e
    rm -rf "$tmp_dir"
    if ! [ -z "$container" ]; then docker rm $container; fi
    # docker rmi $tmp_image || true
}
trap cleanup EXIT INT

opam_tag="${1:-$opam_tag}"

if ! [ -f "$build_dir"/opam-$opam_tag.tgz ]; then
    echo
    echo "### Fetching opam-$opam_tag.tar.gz ..."
    echo
    mkdir -p "$build_dir"
    wget -O "$build_dir"/opam-$opam_tag.tgz \
         https://github.com/ocaml/opam/archive/$opam_tag.tar.gz
fi

echo
echo "### Building opam ..."
echo

cp -a "$build_dir"/opam-$opam_tag.tgz "$tmp_dir"

cat <<EOF > "$tmp_dir"/Dockerfile
FROM ocaml/opam:alpine-3.6_ocaml-4.06.0
ENV PACKAGER "Tezos <ci@tezos.com>"
COPY opam-$opam_tag.tgz opam-$opam_tag.tgz
RUN tar xzf opam-$opam_tag.tgz && \
    cd opam-$opam_tag && \
    ./configure && \
    make lib-ext && \
    make
EOF

docker build -t $tmp_image "$tmp_dir"

container=$(docker create $tmp_image)
docker cp -L $container:/home/opam/opam-$opam_tag/opam "$build_dir"/opam-$opam_tag
docker cp -L $container:/home/opam/opam-$opam_tag/opam-installer "$build_dir"/opam-installer-$opam_tag
