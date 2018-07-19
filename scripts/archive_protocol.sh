#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

if ! [ -z "$(git status -s)" ] ; then
    echo "This script cannot be applied within a dirty git directory,"
    echo "you need 'stash' or 'commit' your changes before."
    exit 1
fi

version=$1
name=${2:-alpha}

if [ -z "$version" ] ; then
    echo "Usage: $0 NNN [alpha|genesis|...]"
    exit 1
fi

alpha_tmpdir=`mktemp -d`

cleanup () {
    set +e
    echo Cleaning up...
    [ ! -d "$alpha_tmpdir" ] || rm -rf "$alpha_tmpdir"
}
trap cleanup EXIT INT

mkdir "$alpha_tmpdir"/src

current_hash_alpha=`jq '.hash' < src/proto_alpha/lib_protocol/src/TEZOS_PROTOCOL | tr -d '"'`

echo "Computing the protocol hash..."

cp src/proto_${name}/lib_protocol/src/*.ml src/proto_${name}/lib_protocol/src/*.mli "$alpha_tmpdir"/src/
grep -v '"hash"' < src/proto_${name}/lib_protocol/src/TEZOS_PROTOCOL > "$alpha_tmpdir"/src/TEZOS_PROTOCOL
long_hash=`./tezos-protocol-compiler $alpha_tmpdir/tmp $alpha_tmpdir/src | cut -d' ' -f2`
short_hash=$(echo $long_hash | head -c 8)

if [ -d "src/proto_${version}_${short_hash}" ] ; then
    echo "Error: you should remove the directory 'src/proto_${version}_${short_hash}'"
    exit 1
fi

git mv src/proto_${name}/ src/proto_${version}_${short_hash}
git commit -m "Archive_protocol: rename proto_${name} into proto_${version}_${short_hash}"

sed -i --follow-symlink \
    -e s/_${name}/_${version}_${short_hash}/g \
    -e s/-${name}/-${version}-${short_hash}/g \
    $(find -name jbuild -or -name \*.opam)

cd "src/proto_${version}_${short_hash}"

rename s/${name}/${version}-${short_hash}/ $(find -name \*.opam)
rename s/_${name}/_${version}_${short_hash}/ $(find -name main_\*.ml -or -name main_\*.mli)

sed -i --follow-symlink \
    -e s/Tezos_protocol_${name}/Tezos_protocol_${version}_${short_hash}/ \
    $(find -name \*.ml -or -name \*.mli) \
    ../proto_genesis/lib_client/proto_alpha.ml \
    ../lib_shell/bench/helpers/proto_alpha.ml

sed -i --follow-symlink \
    -e 's/let version_value = "[^"]*"/let version_value = "${name}_'${version}'"/' lib_protocol/src/raw_context.ml

sed -i --follow-symlink \
    -e 's/"hash": "[^"]*",/"hash": "'$long_hash'",/' \
    lib_protocol/src/TEZOS_PROTOCOL

cd ../..

sed -i --follow-symlink \
    -e "s/${name}/${version}-${short_hash}/" \
    active_protocol_versions

find src/bin_client docs -type f -exec sed "s/$current_hash_alpha/$long_hash/g" -i {} \;

git add .
git commit -m "Archive_protocol: update hashes in proto_${version}_${short_hash}"
