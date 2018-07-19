#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

branch=$1
has_git() {
    which git && [ -d .git ]
}

if has_git && ! [ -z "$(git status -s)" ] ; then
    echo "This script cannot be applied within a dirty git directory,"
    echo "you need 'stash' or 'commit' your changes before."
    exit 1
fi

set -e

current_hash_genesis=`jq '.hash' < src/proto_genesis/lib_protocol/src/TEZOS_PROTOCOL | tr -d '"'`
echo "Genesis's current hash: $current_hash_genesis"
genesis_tmpdir=`mktemp -d`
mkdir $genesis_tmpdir/src
cp src/proto_genesis/lib_protocol/src/*.ml src/proto_genesis/lib_protocol/src/*.mli $genesis_tmpdir/src/
grep -v '"hash"' < src/proto_genesis/lib_protocol/src/TEZOS_PROTOCOL > $genesis_tmpdir/src/TEZOS_PROTOCOL
new_hash_genesis=`./tezos-protocol-compiler -hash-only $genesis_tmpdir/tmp $genesis_tmpdir/src`
echo "Genesis's new hash: $new_hash_genesis"
if [ "$current_hash_genesis" != "$new_hash_genesis" ]
then
    find . -type f -exec sed "s/$current_hash_genesis/$new_hash_genesis/g" -i {} \;
    git commit -a -m "Update proto Genesis's hash"
else
    echo "Proto Genesis's hash hasn't changed, nothing to do"
fi

current_hash_alpha=`jq '.hash' < src/proto_alpha/lib_protocol/src/TEZOS_PROTOCOL | tr -d '"'`
echo "Alpha's current hash: $current_hash_alpha"
alpha_tmpdir=`mktemp -d`
mkdir $alpha_tmpdir/src
cp src/proto_alpha/lib_protocol/src/*.ml src/proto_alpha/lib_protocol/src/*.mli $alpha_tmpdir/src/
grep -v '"hash"' < src/proto_alpha/lib_protocol/src/TEZOS_PROTOCOL > $alpha_tmpdir/src/TEZOS_PROTOCOL
new_hash_alpha=`./tezos-protocol-compiler -hash-only $alpha_tmpdir/tmp $alpha_tmpdir/src`
echo "Alpha's new hash: $new_hash_alpha"
if [ "$current_hash_alpha" != "$new_hash_alpha" ]
then
    find src/proto_alpha src/bin_client docs -type f -exec sed "s/$current_hash_alpha/$new_hash_alpha/g" -i {} \;
    git commit -a -m "Update proto Alpha's hash"
else
    echo "Proto Alpha's hash hasn't changed, nothing to do"
fi
