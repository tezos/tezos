#! /bin/sh

set -e
set -x

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$ci_dir"/docker_registry.inc.sh

username="$1"
password="$2"
reponame="${3##registry.gitlab.com/}"
shift 3
tags="${@:-latest}"

for tag in $tags; do

    scope='push,pull'
    token="$(getBearerToken "${username}" "${password}" "${reponame}")"
    if [ -z "$token" ]; then
        echo "Failed to fetch the Bearer token"
        exit 1
    fi
    digest="$(getTagDigest "${token}" "${reponame}" "${tag}")"
    if [ -z "$digest" ]; then
        echo "Failed to locate the ${reponame}:${tag}"
        exit 1
    fi

    scope='*'
    token="$(getBearerToken "${username}" "${password}" "${reponame}")"
    if [ -z "$token" ]; then
        echo "Failed to fetch the Bearer token"
        exit 1
    fi
    deleteDigest "${token}" "${reponame}" "${digest}"

done
