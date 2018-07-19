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
dir_name=${3:-$name}
lib_name=`echo $dir_name | tr -- _ -`

if [ -z "$version" ] ; then
    echo "Usage: $0 NNN alpha [proto_dir]"
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

current_hash_alpha=`jq '.hash' < src/proto_$dir_name/lib_protocol/src/TEZOS_PROTOCOL | tr -d '"'`

echo "Computing the protocol hash..."

cp src/proto_${dir_name}/lib_protocol/src/*.ml src/proto_${dir_name}/lib_protocol/src/*.mli "$alpha_tmpdir"/src/
sed -i --follow-symlink \
    -e 's/let version_value = "[^"]*"/let version_value = "'${name}'_'${version}'"/' \
    "$alpha_tmpdir"/src/raw_context.ml
grep -v '"hash"' < src/proto_${dir_name}/lib_protocol/src/TEZOS_PROTOCOL > "$alpha_tmpdir"/src/TEZOS_PROTOCOL
long_hash=`./tezos-protocol-compiler -hash-only  $alpha_tmpdir/tmp $alpha_tmpdir/src`
short_hash=$(echo $long_hash | head -c 8)

if [ -d "src/proto_${version}_${short_hash}" ] ; then
    echo "Error: you should remove the directory 'src/proto_${version}_${short_hash}'"
    exit 1
fi

git mv src/proto_${dir_name}/ src/proto_${version}_${short_hash}
git commit -m "Archive_protocol: rename proto_${dir_name} into proto_${version}_${short_hash}"

sed -i --follow-symlink \
    -e s/_${dir_name}/_${version}_${short_hash}/g \
    -e s/-${lib_name}/-${version}-${short_hash}/g \
    $(find -name jbuild -or -name \*.opam)

if ls src/proto_000_*/lib_protocol/src/TEZOS_PROTOCOL | grep proto_000_ > /dev/null ; then
    proto_genesis_dir="proto_000_`ls src | grep proto_000_ | cut -f3 -d_`"
else
    proto_genesis_dir="proto_genesis"
fi

cd "src/proto_${version}_${short_hash}"

rename s/${lib_name}/${version}-${short_hash}/ $(find -name \*.opam)
rename s/_${dir_name}/_${version}_${short_hash}/ $(find -name main_\*.ml -or -name main_\*.mli)

sed -i --follow-symlink \
    -e s/Tezos_protocol_${dir_name}/Tezos_protocol_${version}_${short_hash}/ \
    $(find -name \*.ml -or -name \*.mli) \
    ../$proto_genesis_dir/lib_client/proto_alpha.ml \
    ../lib_shell/bench/helpers/proto_alpha.ml

sed -i --follow-symlink \
    -e 's/let name = "[^"]*"/let name = "'${name}'_'${version}'"/' \
    lib_client/proto_alpha.ml \
    lib_protocol/test/helpers/proto_alpha.ml

sed -i --follow-symlink \
    -e 's/let version_value = "[^"]*"/let version_value = "'${name}'_'${version}'"/' \
    lib_protocol/src/raw_context.ml

sed -i --follow-symlink \
    -e 's/"hash": "[^"]*",/"hash": "'$long_hash'",/' \
    lib_protocol/src/TEZOS_PROTOCOL

if [ $proto_genesis_dir = "proto_genesis" ] ; then
    sed -i --follow-symlink \
        -e "s/-genesis/-000-Ps9mPmXa/" \
        -e "s/_genesis/_000_Ps9mPmXa/" \
        $(find lib_delegate/test -type f)
fi

cd ../..

sed -i --follow-symlink \
    -e "s/${lib_name}/${version}-${short_hash}/" \
    active_protocol_versions

find src/bin_client docs -type f -exec sed "s/$current_hash_alpha/$long_hash/g" -i {} \;

git add .
git commit -m "Archive_protocol: update hashes in proto_${version}_${short_hash}"
