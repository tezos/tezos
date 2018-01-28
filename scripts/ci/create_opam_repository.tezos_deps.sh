#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

build_dir=${build_dir:-_docker_build}

if [ -f "$build_dir/opam_repository-tezos_deps-$ocaml_version.tgz" ]; then
    exit 0
fi

tmp_image="opam_bundle-tezos"
tmp_dir=$(mktemp -dt tezos.opam_bundle.XXXXXXXX)

cleanup () {
    set +e
    rm -rf "$tmp_dir"
    if ! [ -z "$container" ]; then docker rm $container; fi
    # docker rmi $tmp_image || true
}
trap cleanup EXIT INT


## Creating a repository of tezos packages

repo="$tmp_dir"/opam-repository-tezos
opams=$(find "$src_dir" -name \*.opam -print)
mkdir -p "$repo/packages"
echo "1.2" > "$repo/version"
packages=
for opam in $opams; do
    dir=$(dirname $opam)
    file=$(basename $opam)
    package=${file%.opam}
    destdir="$repo/packages/$package/$package.dev"
    mkdir -p "$destdir"
    cp -a "$opam" "$destdir/opam"
    # echo "local: \"rsync://$dir\"" > "$destdir/url"
    packages="$packages $package"
done

if ! [ -f "$build_dir"/opam-repository-master.tgz ]; then
    echo
    echo "### Fetching opam-repository-master.tar.gz ..."
    echo
    mkdir -p "$build_dir"
    wget -O "$build_dir"/opam-repository-master.tgz \
         https://github.com/ocaml/opam-repository/archive/master.tar.gz
fi
tar -C "$tmp_dir" -xzf "$build_dir"/opam-repository-master.tgz

## HACK: Once opam2 is released, we should use the `ocaml/opam` image
## instead of this custom installation of ocaml and opam.

"$ci_dir"/create_binary.opam.sh
cp -a "$build_dir"/opam-$opam_tag "$tmp_dir"/opam

echo
echo "### Building tezos_bundle.tar.gz..."
echo

cat <<EOF > "$tmp_dir"/Dockerfile
FROM alpine
ENV PACKAGER "Tezos <ci@tezos.com>"
COPY opam-repository-master opam-repository-master
COPY opam /usr/local/bin/opam
RUN apk add --no-cache ocaml build-base m4 tar xz bzip2 curl perl rsync
RUN cd ./opam-repository-master/compilers && \
    ( ls -1 | grep -v \$(ocamlc --version) | xargs rm -r )
RUN opam init --no-setup --yes default ./opam-repository-master
RUN opam install --yes opam-bundle
COPY opam-repository-tezos opam-repository-tezos
RUN opam bundle --yes --output="tezos_bundle-$ocaml_version" \
                --repository=opam-repository-tezos \
                --repository=opam-repository-master \
                --ocaml=$ocaml_version \
                $packages depext ocp-indent
EOF

docker build --pull -t $tmp_image "$tmp_dir"

container=$(docker create $tmp_image)
docker cp -L $container:/tezos_bundle-$ocaml_version.tar.gz "$tmp_dir"

cd "$tmp_dir"
tar xf tezos_bundle-$ocaml_version.tar.gz tezos_bundle-$ocaml_version/repo

# removing fake tezos packages
cd tezos_bundle-$ocaml_version/repo/packages
rm -r $packages

# Repacking the repo
cd "$tmp_dir"
mv tezos_bundle-$ocaml_version/repo opam_repository-tezos_deps
tar czf "opam_repository-tezos_deps-$ocaml_version.tgz" \
    opam_repository-tezos_deps

cd "$src_dir"
mv "$tmp_dir"/opam_repository-tezos_deps-$ocaml_version.tgz "$build_dir"
