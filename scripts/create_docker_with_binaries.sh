#! /bin/sh

set -x
set -e

dir="$(mktemp -d)"
src_dir="$(dirname "$(readlink -f "$0")")"

image_name="${1:-tezos_build}"
base_image="${2:-alpine_ocaml-4.03.0}"
image_version="$3"

tezos_build_img="tezos_build:$base_image"

if ! (docker images | grep -- "^tezos_build \+$base_image "); then
  echo "Docker image not found: $tezos_build_img" >&2
  echo "Aborting" >&2
  exit 1
  fi

cd "$dir"

git clone "$src_dir"/.. "$dir"/tezos
rm -fr "$dir"/tezos/.git

cp "$src_dir"/Dockerfile.build_bin.in "$dir"
sed Dockerfile.build_bin.in -e 's/$base_image/'"$base_image"'/g' > Dockerfile.build_bin

docker build -f Dockerfile.build_bin -t "tezos_build_bin:$base_image$image_version" "$dir"

mkdir -p "$dir"/built-bin
docker run -i --rm -v "$dir"/built-bin:/built-bin "tezos_build_bin:$base_image$image_version" /bin/bash << EOF
sudo cp -v /home/opam/bin/tezos-* /built-bin/
sudo chown opam:nogroup /built-bin/tezos-*
sudo chmod a+rwx /built-bin/tezos-*
EOF

cp "$src_dir"/Dockerfile.binaries.in "$dir"
sed Dockerfile.binaries.in -e 's/$base_image/'"$base_image"'/g' > Dockerfile.binaries

docker build -f Dockerfile.binaries -t "tezos_binaries:$base_image$image_version" "$dir"
