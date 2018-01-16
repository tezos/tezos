#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. scripts/version.sh

image_name="${1:-tezos/opam}"
image_version="${2:-alpine-${alpine_version}_ocaml-${ocaml_version}}"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf Dockerfile
}
trap cleanup EXIT INT

apk_keys="_apk/keys/"
apk_packages="_apk/packages/home/x86_64/"

if ! ( [ -f "$apk_packages/leveldb-$leveldb_version-r0.apk" ] && \
       [ -f "$apk_packages/leveldb-dev-$leveldb_version-r0.apk" ] && \
       [ -d "$apk_keys" ] ) ; then
  ./scripts/create_apk.leveldb.sh
fi

opam_tag=2.0.0-rc

cat <<EOF > Dockerfile
FROM alpine:$alpine_version

RUN apk update && \
    apk upgrade && \
    apk --no-cache add su-exec build-base \
            bash ncurses-dev xz m4 git ca-certificates wget \
            gmp-dev libev-dev libressl-dev pcre-dev perl zlib-dev libsodium-dev && \
    update-ca-certificates && \
    rm -f /var/cache/apk/* && \
    adduser -S opam

COPY $apk_keys /etc/apk/keys/
COPY $apk_packages/leveldb-$leveldb_version-r0.apk .
COPY $apk_packages/leveldb-dev-$leveldb_version-r0.apk .

RUN apk --no-cache add leveldb-$leveldb_version-r0.apk \
                       leveldb-dev-$leveldb_version-r0.apk && \
    rm -f /var/cache/apk/* \
          leveldb-$leveldb_version-r0.apk \
          leveldb-dev-$leveldb_version-r0.apk

RUN git clone --depth 1 -b "$opam_tag" git://github.com/ocaml/opam && \
    cd opam && \
    make cold CONFIGURE_ARGS="--prefix /usr" && \
    make install && \
    cd .. && \
    rm -r opam

USER opam

WORKDIR /home/opam

RUN mkdir ~/.ssh && \
    chmod 700 ~/.ssh && \
    git config --global user.email "tezos@example.com" && \
    git config --global user.name "Tezos CI" && \
    cd /home/opam && \
    git clone --depth 1 -b master git://github.com/ocaml/opam-repository && \
    cd opam-repository && \
    cd /home/opam/opam-repository && \
    opam admin upgrade && \
    git checkout -b v2 && \
    git add . && \
    git commit -a -m 'opam admin upgrade' && \
    opam init -a -y --comp $ocaml_version /home/opam/opam-repository && \
    opam install -y depext

EOF

echo
echo "### Building base image..."
echo

docker build --pull -t "$image_name:$image_version" .

rm Dockerfile
