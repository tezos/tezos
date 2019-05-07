#! /bin/bash

set -e

usage="Usage:
$ ./scripts/snapshot_alpha.sh babylon_005 from athens_004
Packs the current proto_alpha directory in a new proto_005_<hash>
directory with all the necessary renamings.

$ ./scripts/snapshot_alpha.sh babylon_005 --master
Prepares the protocol for master, just for development."

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

current=$1
label=$(echo $current | cut -d'_' -f1)
version=$(echo $current | cut -d'_' -f2)

if ! ( [[ "$label" =~ ^[a-z]+$ ]] && [[ "$version" =~ ^[0-9][0-9][0-9]$ ]] ); then
    echo "Wrong protocol version"
    echo
    echo "$usage"
    exit 1
fi

predecessor=$3
previous_label=$(echo $predecessor | cut -d'_' -f1)
previous_version=$(echo $predecessor | cut -d'_' -f2)
if ! ( [[ "$2" == "from" ]] && [[ "$3" ]] && [[ "$previous_label" =~ ^[a-z]+$ ]] && [[ "$previous_version" =~ ^[0-9][0-9][0-9]$ ]] ); then
    if [[ "$2" == "--master" ]]; then master="true"
    else
        echo 'pass a predecessor such as "from athens_004" or "--master"'
        echo
        echo "$usage"
        exit 1
    fi
fi

if [ -d src/proto_${version} ] ; then
    echo "Error: you should remove the directory 'src/proto_${version}'"
    exit 1
fi

# create a temporary directory until the hash is known
# this is equivalent to `cp src/proto_alpha/ src/proto_${version}` but only for versioned files
mkdir /tmp/tezos_proto_snapshot
git archive HEAD src/proto_alpha/ | tar -x -C /tmp/tezos_proto_snapshot
mv /tmp/tezos_proto_snapshot/src/proto_alpha src/proto_${version}
rm -rf /tmp/tezos_proto_snapshot

# set current version
sed -i.old.old -e 's/let version_value = "alpha_current"/let version_value = "'${current}'"/' \
    src/proto_${version}/lib_protocol/raw_context.ml

# set previous version
if [[ "$master" ]]; then
    #in master our predecessor is alpha_current
    sed -i.old -e 's/s = "alpha_previous"/s = "alpha_current"/' \
        src/proto_${version}/lib_protocol/raw_context.ml
else
    # set previous version
    Predecessor=$(echo $predecessor | sed 's/.*/\u&/') # capitalize
    sed -i.old -e 's/Alpha_previous/'${Predecessor}'/' \
        src/proto_${version}/lib_protocol/{raw_context.ml,raw_context.mli,init_storage.ml}

    # set previous version
    sed -i.old -e 's/s = "alpha_previous"/s = "'${predecessor}'"/' \
        src/proto_${version}/lib_protocol/raw_context.ml
fi

long_hash=$(./tezos-protocol-compiler -hash-only src/proto_${version}/lib_protocol)
short_hash=$(echo $long_hash | head -c 8)

if [ -d src/proto_${version}_${short_hash} ] ; then
    echo "Error: you should remove the directory 'src/proto_${version}_${short_hash}'"
    exit 1
fi

mv src/proto_${version} src/proto_${version}_${short_hash}

cd src/proto_${version}_${short_hash}/lib_protocol

# replace fake hash with real hash, this file doesn't influence the hash
sed -i.old -e 's/"hash": "[^"]*",/"hash": "'$long_hash'",/' \
    TEZOS_PROTOCOL

# this is a safe pattern that we can replace everywhere
sed -i.old -e s/protocol_alpha/protocol_${version}_${short_hash}/ \
    $(find . -type f)

# opam files
sed -i.old -e 's/alpha/'${version}_${short_hash}'/' \
    tezos{,-embedded}-protocol-alpha.opam

rename s/alpha/${version}-${short_hash}/ $(find . -name \*.opam)

# fix content of dune files
sed -i.old -e 's/\\"alpha\\"/\\"'${version}_${short_hash}'\\"/' \
       -e 's/"alpha"/"'${version}_${short_hash}'"/' \
       -e s/_alpha/_${version}_${short_hash}/g \
       -e s/-alpha/-${version}-${short_hash}/g \
    $(find . -name dune -or -name dune.inc)


# move deamons to a tmp directory to avoid editing lib_protocol
cd ..
daemons=$(ls | grep -v lib_protocol)
mkdir tmp
mv $daemons tmp
cd tmp

sed -i.old -e 's/"alpha"/"'${version}-${short_hash}'"/' \
       -e 's/_alpha/_'${version}_${short_hash}'/' \
       lib_client/proto_alpha.ml

# rename main_*.ml{,i} files of the binaries
rename s/_alpha/_${version}_${short_hash}/ $(find . -name main_\*.ml -or -name main_\*.mli)

# rename .opam files
rename s/alpha/${version}-${short_hash}/ $(find . -name \*.opam)

# fix content of dune and opam files
sed -i.old -e s/_alpha/_${version}_${short_hash}/g \
       -e s/-alpha/-${version}-${short_hash}/g \
    $(find . -name dune -or -name \*.opam)

# rename genesis except if in master
if [[ ! "$master" ]]; then
    #rename genesis
    sed -i.old -e "s/-genesis/-000-Ps9mPmXa/" \
        $(find . -name dune -or -name \*.opam)

    sed -i.old -e "s/_genesis/_000_Ps9mPmXa/" \
        $(find lib_delegate/test -type f)
fi

mv $daemons ..
cd ..
rmdir tmp
find . -name '*.old' -exec rm {} \;
