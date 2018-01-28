#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$ci_dir"/docker_registry.inc.sh

username="$1"
password="$2"
image="${3##registry.gitlab.com/}"

reponame=${image%%:*}
tag=${image##*:}

token="$(getBearerToken "${username}" "${password}" "${reponame}")"
if [ -z "$token" ]; then
    echo "Failed to fetch the Bearer token"
    exit 1
fi

readTag "$token" "$reponame" "$tag"
