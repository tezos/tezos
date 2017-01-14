#! /bin/sh

set -x

dir=$(mktemp -d)
cur_dir="$(dirname "$(readlink -f "$0")")"

image_name=${1:=tezos_build}
ocaml_version=${2:=alpine_ocaml-4.03.0}
image_version=$3

docker pull ocaml/opam:${ocaml_version}

cp ${cur_dir}/install_build_deps.sh ${dir}
cp ${cur_dir}/../src/tezos-deps.opam ${dir}
cat > ${dir}/Dockerfile <<EOF
FROM ocaml/opam:${ocaml_version}
COPY install_build_deps.sh /tmp
COPY tezos-deps.opam /tmp/src/tezos-deps.opam
RUN cd /tmp && opam config exec -- ./install_build_deps.sh pin \
            && rm -fr ~/.opam/log/
USER root
ENV HOME /home/opam
RUN cd /tmp && opam config exec -- ./install_build_deps.sh depext \
            && rm -fr ~/.opam/log/
RUN apk add libsodium-dev
USER opam
RUN cd /tmp && opam config exec -- ./install_build_deps.sh install \
            && rm -fr ~/.opam/log/
EOF

docker build -t ${image_name}:${ocaml_version}${image_version} ${dir}
