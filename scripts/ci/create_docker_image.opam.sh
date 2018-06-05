#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

export build_dir=${build_dir:-_docker_build}
tmp_dir=$(mktemp -dt tezos.opam.tezos.XXXXXXXX)

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf "$tmp_dir"
}
trap cleanup EXIT INT

"$ci_dir"/create_apk.leveldb.sh
"$ci_dir"/create_apk.hidapi.sh
"$ci_dir"/create_binary.opam.sh
"$ci_dir"/create_opam_repository.tezos_deps.sh

echo
echo "### Building base image..."
echo

image_name="${1:-tezos_opam}"
image_version="${2:-alpine-${alpine_version}_ocaml-${ocaml_version}}"

cp -a "$build_dir"/leveldb-$leveldb_version-r0.apk \
      "$build_dir"/leveldb-dev-$leveldb_version-r0.apk \
      "$build_dir"/keys/ \
      "$tmp_dir"

cp -a "$build_dir"/hidapi-$hidapi_version-r0.apk \
      "$build_dir"/hidapi-dev-$hidapi_version-r0.apk \
      "$build_dir"/keys/ \
      "$tmp_dir"

cp "$build_dir"/opam-$opam_tag "$tmp_dir/opam"
cp "$build_dir"/opam-installer-$opam_tag "$tmp_dir/opam-installer"

tar -C "$tmp_dir" -xzf "$build_dir"/opam_repository-tezos_deps-$ocaml_version.tgz

cat <<EOF > "$tmp_dir"/Dockerfile
FROM alpine:$alpine_version

COPY opam /usr/bin/opam
COPY opam-installer /usr/bin/opam-installer

COPY keys /etc/apk/keys/
COPY leveldb-$leveldb_version-r0.apk .
COPY leveldb-dev-$leveldb_version-r0.apk .
COPY hidapi-$hidapi_version-r0.apk .
COPY hidapi-dev-$hidapi_version-r0.apk .


RUN apk --no-cache add \
        build-base bash perl xz m4 git curl tar rsync patch sudo jq \
        ncurses-dev gmp-dev libev-dev \
        pcre-dev zlib-dev \
        snappy snappy-dev \
        leveldb-$leveldb_version-r0.apk \
        leveldb-dev-$leveldb_version-r0.apk \
        hidapi-$hidapi_version-r0.apk \
        hidapi-dev-$hidapi_version-r0.apk && \
    adduser -S opam && \
    echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam && \
    chmod 440 /etc/sudoers.d/opam && \
    chown root:root /etc/sudoers.d/opam && \
    sed -i.bak 's/^Defaults.*requiretty//g' /etc/sudoers
USER opam

WORKDIR /home/opam

COPY opam_repository-tezos_deps opam-repository-tezos_deps

RUN mkdir ~/.ssh && \
    chmod 700 ~/.ssh && \
    git config --global user.email "ci@tezos.com" && \
    git config --global user.name "Tezos CI" && \
    opam init --bare --no-setup --yes \
              tezos_deps /home/opam/opam-repository-tezos_deps && \
    opam switch create --yes tezos ocaml-base-compiler.${ocaml_version}

RUN opam install --yes opam-depext

ENTRYPOINT [ "opam", "exec", "--" ]
CMD [ "/bin/bash" ]
EOF

docker build --pull -t "$image_name:$image_version" "$tmp_dir"
