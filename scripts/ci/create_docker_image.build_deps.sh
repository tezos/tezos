#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

export build_dir=${build_dir:-_docker_build}
tmp_dir=$(mktemp -dt tezos.build_deps.tezos.XXXXXXXX)

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf "$tmp_dir"
}
trap cleanup EXIT INT

opam_files=$(find -name \*.opam | sort)
dependencies="$opam_files scripts/install_build_deps.sh scripts/version.sh scripts/opam-pin.sh scripts/opam-unpin.sh scripts/opam-remove.sh"

image_name="${1:-tezos_build_deps}"
image_version="${2:-latest}"
base_image="${3:-tezos_opam:alpine-${alpine_version}_ocaml-${ocaml_version}}"
cached_image="${4:-}"

cat <<EOF > "$tmp_dir"/Dockerfile
FROM $base_image
COPY . tezos
RUN sudo chown -R opam tezos
RUN opam exec -- ./tezos/scripts/install_build_deps.sh || \
    ( echo ; \
      echo "### Failed to build with preloaded package" ; \
      echo "### Retrying with the default opam repository" ; \
      echo ; \
      opam remote add default https://opam.ocaml.org/2.0 && \
      opam exec -- ./tezos/scripts/install_build_deps.sh )
RUN opam install --yes ocp-indent
EOF

tar -c $dependencies | tar -C "$tmp_dir" -x


## Lookup for for prebuilt dependencies...

if [ ! -z "$cached_image" ]; then
    cd "$tmp_dir"
    base_image_sha1=$("$ci_dir"/docker_registry_read.sh \
                               "${CI_REGISTRY_USER}" "${CI_REGISTRY_PASSWORD}" \
                               "$base_image")
    dependencies_sha1=cache_$(echo "$base_image_sha1" | sha1sum - $dependencies Dockerfile | sha1sum | tr -d ' -')
    echo
    echo "### Looking for prebuilt dependencies ($image_name:$dependencies_sha1)..."
    if "$ci_dir"/docker_registry_tag.sh gitlab-ci-token "${CI_BUILD_TOKEN}" \
                "$image_name" "$dependencies_sha1" "$image_version" ; then
        echo
        echo "### Found $image_name:$dependencies_sha1"
        echo
        exit 0
    fi
    echo
    echo "### Missing..."
    echo
fi

echo
echo "### Building tezos dependencies..."
echo

docker build -t "$image_name:$image_version" "$tmp_dir"

echo
echo "### Succesfully build docker image: $image_name:$image_version"
echo

if [ ! -z "$cached_image" ]; then
    echo
    echo "### Saving docker image ($image_name:$dependencies_sha1)..."
    echo
    docker push "$image_name:$image_version"
    "$ci_dir"/docker_registry_tag.sh \
             "${CI_REGISTRY_USER}" "${CI_REGISTRY_PASSWORD}" \
             "$image_name" "$image_version" "$dependencies_sha1"
fi
