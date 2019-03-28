#! /bin/bash

set -e

usage="Usage:
$ ./scripts/activate_protocol.sh src/proto_004_PtDPBVyN
Inserts the protocol in the right files of the build system to compile it
If in master activates in addition to alpha.
If in mainnet activates in addition to its predecessor, here proto_003_PsddFKi3."

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

if [ ! -d "$1" ]; then
    echo "$usage"
    exit 1
fi

is_mainnet () {
    # need to check a real file because of phantom git directories
    if [ -f "src/proto_000_Ps9mPmXa/lib_protocol/TEZOS_PROTOCOL" ]
    then return 0; else return 1; fi
}

new_version=$(basename $1 | awk -F'_' '{print $2}')
new_hash=$(basename $1 | awk -F'_' '{print $3}')
full_hash=$(jq .hash < $1/lib_protocol/TEZOS_PROTOCOL)
replacement=${new_version}-${new_hash}
if [[ -z "${new_version}" || -z "${new_hash}" || -z "${full_hash}" ]] ; then
    echo "$usage"
    exit 1
fi

# The pattern to look for, "alpha" for master or "00X-<hash>" for mainnet.
# Once found it's either replaced or the line is duplicated and then replaced
if is_mainnet
then
    old_version=$( printf '%03d' $(($new_version -1)) )
    old_dir=$(ls -d src/proto_${old_version}_*)
    old_hash=$(basename $old_dir | awk -F'_' '{print $3}')
    pattern=${old_version}-${old_hash}
else
    pattern="alpha"
fi

# if a line matches PATTERN, a new line is printed where the pattern is replaced
duplicate_and_replace() {
    PATTERN=$1
    REPLACEMENT=$2
    shift 2

    awk -i inplace '{
        print
        if ($0 ~ PATTERN) {
           sub(PATTERN,REPLACEMENT)
           print
        }}' PATTERN=$PATTERN REPLACEMENT=$REPLACEMENT $*
}

# the minimum needed, although you can't bake
duplicate_and_replace ${pattern} ${replacement} active_protocol_versions

# activate in client to bake and use RPCs
duplicate_and_replace -${pattern} -${replacement} \
    src/bin_client/{dune,tezos-client.opam}

read -p "Link in the Node? (no if you want to test injection) (Y/n) " ans
if [[ "$ans" == "Y" || "$ans" == "y" || -z "$ans" ]]; then
    duplicate_and_replace -${pattern} -${replacement} \
                          src/bin_node/{dune,tezos-node.opam}
fi

read -p "User-activated update in 3 blocks? (Y/n) " ans
if [[ "$ans" == "Y" || "$ans" == "y" || -z "$ans" ]]; then
    # clean existing lines, if any
    awk -i inplace '
    BEGIN{found=0}{
    if (!found && $0 ~ "let forced_protocol_upgrades")
      {found=1; print}
      else {
        if (found && $0 ~ "^]")
        {found=0; print }
        else
        { if (!found){print}}
       }}' src/lib_base/block_header.ml

    sed -i.old '/let forced_protocol_upgrades/ a \ \ 3l, Protocol_hash.of_b58check_exn '${full_hash}' ;' \
        src/lib_base/block_header.ml
    rm src/lib_base/block_header.ml.old
fi
